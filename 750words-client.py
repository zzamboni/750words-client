#!/usr/bin/env python3
def eprint(*eargs, **ekwargs):
    if not args.quiet:
        print(*eargs, file=sys.stderr, **ekwargs)

min_words = 750
max_words = 4995

import argparse
import os
import sys
import time

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
parser.add_argument("--no-headless",
                    help="Disable headless mode (opens the Chrome app window).",
                    action="store_true")
parser.add_argument("--quiet",
                    help="Don't print progress messages.",
                    action="store_true")
args = parser.parse_args()

username = os.getenv('USER_750WORDS') or ""
password = os.getenv('PASS_750WORDS') or ""

if not(username and password):
    eprint("Please set the USER_750WORDS/PASS_750WORDS environment variables")
    sys.exit(1)

text = ""
text_count = 0
if not (args.count or args.text):
    for infile in args.FILE:
        text = text + infile.read() + "\n"
    text_count = len(text.split())
    eprint("Got text: " + text + (" (%d words)" % text_count))

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

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

driver = webdriver.Chrome(options=opts)

eprint("Connecting to 750words.com...")
driver.get('https://750words.com/auth')

eprint("Authenticating...")
login_form = WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.ID, 'signin_form'))
)
if login_form:
    user_field = driver.find_element_by_id('person_email_address')
    password_field = driver.find_element_by_id('person_password')
    user_field.send_keys(username)
    password_field.send_keys(password)
    login_form.submit()
else:
    raise BaseException("Could not find login form in https://750words.com/auth")

eprint("Finding current text entry...")
# We use WebDriverWait to wait (with a limit) until the page is loaded and the
# necessary element appears.
# text_field = driver.find_element_by_id('entry_body')
text_field = WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.ID, 'entry_body'))
)
if text_field:
    current_text = text_field.get_attribute("value")
    current_word_count = len(current_text.split())
    if args.count:
        print("Current word count: "+str(current_word_count))
    if args.text:
        print(current_text)
    if not (args.count or args.text):
        enter_text = True
        if (not args.replace) and args.only_if_needed and (current_word_count >= args.min):
            eprint("Word count is already enough, not entering text.")
            enter_text = False
        if enter_text:
            if args.replace:
                eprint("Clearing existing text...")
                text_field.clear()
                current_text = ""
                current_word_count = 0
            if (current_word_count+text_count) > args.max:
                new_word_count = args.max - current_word_count
                eprint("Trimming new text to %d words to keep total below %d" % (new_word_count, args.max))
                # This is imperfect - line breaks are replaced with spaces
                text = ' '.join(text.split()[:new_word_count])
            eprint("Entering new text...")
            text_field.send_keys(text)
            eprint("Saving...")
            ActionChains(driver).key_down(Keys.COMMAND).send_keys('s').key_up(Keys.COMMAND).perform()
            # If the warning dialog about losing words appears, click "Save
            # anyway"
            warning_dialog_text = driver.find_element_by_xpath('//div[@id="losing_words"]').text
            if warning_dialog_text:
                driver.find_element_by_xpath('//div[@class="ui-dialog-buttonset"]/button[1]').click()
            time.sleep(2)
            new_text = text_field.get_attribute("value")
            new_word_count = len(new_text.split())
            eprint("New word count: %d" % new_word_count)
            # Wait until the "Saved!" floating popup appears.
            # WebDriverWait(driver, 5).until(
            #     EC.presence_of_element_located((By.ID, 'achtung-overlay'))
            # )
else:
    raise BaseException("Could not find text entry form in page.")

eprint("Done!")
driver.quit()
