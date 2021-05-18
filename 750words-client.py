#!/usr/bin/env python3
word_limit = 750

import argparse
import os

parser = argparse.ArgumentParser(description="Update 750words.com from the command line. Input is read from standard input.")
parser.add_argument("--limit",
                    help=("Specify minimum words needed. Default: %d" % word_limit),
                    default=word_limit,
                    type=int)
parser.add_argument("--only-if-needed",
                    help="Only add text if current word count is below the limit.",
                    action="store_true")
parser.add_argument("--replace",
                    help="Replace any current text with the new one, default is to add at the end.",
                    action="store_true")
parser.add_argument("--count",
                    help="Don't upload anything, only print the current word count.",
                    action="store_true")
parser.add_argument("--no-headless",
                    help="Disable headless mode (opens the Chrome app window).",
                    action="store_true")
parser.add_argument("--quiet",
                    help="Don't print progress messages.",
                    action="store_true")
parser.add_argument("--user",
                    help="User name to use for 750words. Can be provided through the USER_750WORDS environment variable.",
                    type=str,
                    default=(os.getenv('USER_750WORDS') or ""))
parser.add_argument("--password",
                    help="Password to use for authentication. Can be provided through the PASS_750WORDS environment variable.",
                    type=str,
                    default=(os.getenv('PASS_750WORDS') or ""))
args = parser.parse_args()

def eprint(*eargs, **ekwargs):
    if not args.quiet:
        print(*eargs, file=sys.stderr, **ekwargs)

import sys
import time

text = ""
if not args.count:
    for line in sys.stdin:
        text = text + line
    eprint("Got text: "+text)

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# Taken from https://www.intricatecloud.io/2019/05/running-webdriverio-tests-using-headless-chrome-inside-a-container/
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

# opts.add_argument("user-agent=" + ua_list[4])
driver = webdriver.Chrome(options=opts)

eprint("Connecting to 750words.com...")
driver.get('https://750words.com/auth')

eprint("Authenticating...")
# login_form = driver.find_element_by_id('signin_form')
login_form = WebDriverWait(driver, 10).until(
    EC.presence_of_element_located((By.ID, 'signin_form'))
)
if login_form:
    user = driver.find_element_by_id('person_email_address')
    password = driver.find_element_by_id('person_password')
    user.send_keys(args.user)
    password.send_keys(args.password)
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
    else:
        enter_text = True
        if (not args.replace) and args.only_if_needed and (current_word_count >= args.limit):
            eprint("Word count is already enough, not entering text.")
            enter_text = False
        if enter_text:
            if args.replace:
                eprint("Clearing existing text...")
                text_field.clear()
            eprint("Entering new text...")
            text_field.send_keys(text)
            eprint("Saving...")
            ActionChains(driver).key_down(Keys.COMMAND).send_keys('s').key_up(Keys.COMMAND).perform()
            # If the warning dialog about losing words appears, click "Save
            # anyway"
            warning_dialog_text = driver.find_element_by_xpath('//div[@id="losing_words"]').text
            if warning_dialog_text:
                driver.find_element_by_xpath('//div[@class="ui-dialog-buttonset"]/button[1]').click()
            time.sleep(1)
            # Wait until the "Saved!" floating popup appears.
            # WebDriverWait(driver, 5).until(
            #     EC.presence_of_element_located((By.ID, 'achtung-overlay'))
            # )
else:
    raise BaseException("Could not find text entry form in page.")

eprint("Done!")
driver.quit()
