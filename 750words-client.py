#!/usr/bin/env python3
import argparse
import os
import sys
import time
import re

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

def eprint(*eargs, **ekwargs):
    if not args.quiet:
        print(*eargs, file=sys.stderr, **ekwargs)

def word_count(text):
    return len(text.split())

def enter_text(driver, field, value):
    driver.execute_script('arguments[0].value=arguments[1];', field, value)

def find_text_field(driver):
    return WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.ID, 'entry_body'))
    )

min_words = 750
max_words = 5000

parser = argparse.ArgumentParser(description="Interact with 750words.com from the command line.",
                                 epilog=("Your 750words.com credentials must be stored in the "
                                         "USER_750WORDS and PASS_750WORDS environment variables."))
parser.add_argument('FILE',
                    help='Input files for text to add. Default is to read from standard input.',
                    type=argparse.FileType('r'),
                    nargs='*',
                    default=[sys.stdin],)
parser.add_argument("--min",
                    help=("Minimum number of words needed. Default: %d." % min_words),
                    default=min_words,
                    type=int)
parser.add_argument("--max",
                    help=("Maximum total number of words allowed. Default: %d." % max_words),
                    default=max_words)
parser.add_argument("--only-if-needed",
                    help="Only add text if current word count is below MIN.",
                    action="store_true")
parser.add_argument("--replace",
                    help="Replace any current text with the new one, default is to add at the end.",
                    action="store_true")
parser.add_argument("--count",
                    help="Don't upload text, only print the current word count.",
                    action="store_true")
parser.add_argument("--text",
                    help="Don't upload text, only print the current text.",
                    action="store_true")
parser.add_argument("--quiet",
                    help="Don't print progress messages.",
                    action="store_true")
debug_options = parser.add_argument_group('debugging options')
debug_options.add_argument("--no-headless",
                           help="Disable headless mode (opens the Chrome app window).",
                           action="store_true")
debug_options.add_argument("--no-quit",
                           help="Don't quit the browser at the end.",
                           action="store_true")
args = parser.parse_args()

username = os.getenv('USER_750WORDS') or None
password = os.getenv('PASS_750WORDS') or None

if not(username and password):
    eprint("Please set the USER_750WORDS/PASS_750WORDS environment variables")
    sys.exit(1)

text = ""
text_count = 0
if not (args.count or args.text):
    for infile in args.FILE:
        text = text + infile.read() + "\n"
    text_count = word_count(text)
    eprint("Got text: " + text + (" (%d words)" % text_count))

opts = Options()
opts.add_argument("--window-size=1200,800")
if not args.no_headless:
    opts.add_argument("--headless")
opts.add_argument("--no-sandbox")
opts.add_argument("--disable-gpu")
opts.add_argument("--verbose")
opts.add_argument("--disable-setuid-sandbox")
opts.add_argument("--disable-dev-shm-usage")
opts.add_argument("--disable-infobars")
opts.add_argument("--disable-popup-blocking")

driver = webdriver.Chrome(options=opts)

eprint("Connecting to 750words.com...")
driver.get('https://750words.com/auth')

eprint("Authenticating...")
login_form = WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.ID, 'signin_form'))
)

if login_form:
    user_field = driver.find_element(By.ID, 'person_email_address')
    password_field = driver.find_element(By.ID,'person_password')
    enter_text(driver, user_field, username)
    enter_text(driver, password_field, password)
    login_form.submit()
else:
    raise BaseException("Could not find login form in https://750words.com/auth")

eprint("Finding current text entry...")
# We use WebDriverWait to wait (with a limit) until the page is loaded and the
# necessary element appears.
# text_field = driver.find_element_by_id('entry_body')
text_field = find_text_field(driver)

if text_field:
    # Get current text and word count
    current_text = text_field.get_attribute("value")
    current_word_count = word_count(current_text)

    # If --count is given, print the word count
    if args.count:
        print("Current word count: "+str(current_word_count))

    # If --text is given, print the text
    if args.text:
        print(current_text)

    # Otherwise, prepare to enter text
    if not (args.count or args.text):
        add_text = True
        # Print current word count also when adding text, but this can be
        # controlled with --quiet
        eprint("Current word count: "+str(current_word_count))
        # If --only-if-needed is used without --replace, we need to check if we
        # already have enough words
        if (not args.replace) and args.only_if_needed and (current_word_count >= args.min):
            eprint("Word count is already enough, not entering text.")
            add_text = False

        # Finally we get to entering new text
        if add_text:
            # First clear the field if --replace was used
            if args.replace:
                eprint("Clearing existing text...")
                current_text = ""
                current_word_count = 0

            # Check if the end text would have more words than the maximum
            # allowed, and in that case trim it down.
            if (current_word_count + text_count) > args.max:
                new_word_count = args.max - current_word_count
                eprint("Trimming new text to %d words to keep total below %d" % (new_word_count, args.max))
                text = ''.join(re.findall(r'\S+\s*', text)[:new_word_count])

            # Enter the new text in the text field
            eprint("Entering new text...")
            enter_text(driver, text_field, current_text + text)
            text_field.send_keys("\n")

            # Send Ctrl-s to force save
            eprint("Saving...")
            text_field.send_keys(Keys.CONTROL, "s")
            time.sleep(1)

            # 750words issues a warning dialog if the word count gets reduced by
            # a lot when saving the text. This might happen with --replace, so
            # we catch it. If the dialog appears, we click "Save anyway". Note
            # that the <div id="losing_words"> element is always there, but
            # normally empty, so we need to check if it contains any text
            # instead of its existence.
            warning_dialog_text = driver.find_element(By.XPATH, '//div[@id="losing_words"]').text
            if warning_dialog_text:
                eprint("Got the reduced-word-count warning dialog, clicking 'Save anyway'")
                # Press Enter to select the default button, which is "Save anyway"
                driver.switch_to.active_element.send_keys(Keys.ENTER)

            eprint("Reloading page to ensure save succeeded")
            # Disable "Are you sure?" alert on reload
            driver.execute_script("window.onbeforeunload = function() {};")
            driver.refresh()
            time.sleep(1)

            # Get new text and word count
            text_field = find_text_field(driver)
            new_text = text_field.get_attribute("value")
            new_word_count = word_count(new_text)
            eprint("New word count: %d" % new_word_count)
            if new_word_count >= args.min:
                eprint("You completed your %d words for today!" % args.min)
else:
    raise BaseException("Could not find text entry form in page.")

eprint("Done!")
if not args.no_quit:
    driver.quit()
