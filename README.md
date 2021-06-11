

# 750words command-line client

A simple client for posting text to your [750words.com](https://750words.com/) account. Can also be used to query your current word count or the current text. Emacs integration is also available.

You can see its full annotated source code in the [750words-client.org](https://github.com/zzamboni/750words-client/blob/main/750words-client.org) file.


# Table of Contents

1.  [750words command-line client](#org6703862)
    1.  [Command line client](#command-line-client)
        1.  [Installation](#installation)
        2.  [Usage](#usage)
        3.  [Client Implementation](#implementation)
    2.  [Emacs integration](#emacs-integration)
        1.  [Installation](#emacs-installation)
        2.  [Usage](#emacs-usage)


<a id="command-line-client"></a>

## Command line client


<a id="installation"></a>

### Installation

1.  Docker image

    [[750words-client on Docker Hub](https://hub.docker.com/r/zzamboni/750words-client)][[Dockerfile](https://github.com/zzamboni/750words-client/blob/main/Dockerfile)]
    
    You can use `750words-client.py` from its Docker image as follows (the image will be downloaded from [Docker Hub](https://hub.docker.com/r/zzamboni/750words-client) the first time you use it):
    
        docker run zzamboni/750words-client --help
    
    Note that you have to define the `USER_750WORDS` and `PASS_750WORDS` environment variables in your environment, and pass them to the container. You also need to pass the `-i` option to `docker run` if you want to read the input from standard input, e.
    
        cat file.txt | docker run -i -e USER_750WORDS -e PASS_750WORDS zzamboni/750words-client
    
    If you want to build the image yourself, you can do it as follows from a checkout of its [git repository](https://github.com/zzamboni/750words-client):
    
        docker build --tag 750words-client .

2.  Installation from source

    [[GitHub repository](https://github.com/zzamboni/750words-client)]
    
    Clone the git repository:
    
        git clone https://github.com/zzamboni/750words.git
    
    You need the following libraries and components installed:
    
    -   [Selenium](https://selenium-python.readthedocs.io/) Python bindings (run `pip install -r requirements.txt`)
    -   [Google Chrome](https://www.google.com/chrome/) is used to automate the connections.
    -   [ChromeDriver](https://chromedriver.chromium.org/) so that Selenium can connect to Chrome - make sure you install the version that corresponds to the Chrome version you have installed.
    
    You can then copy `750words-client.py` to somewhere in your `$PATH` to use it.


<a id="usage"></a>

### Usage

    750words-client.py --help

    usage: 750words-client.py [-h] [--min MIN] [--max MAX] [--only-if-needed]
                              [--replace] [--count] [--text] [--quiet]
                              [--no-headless] [--no-quit]
                              [FILE ...]
    
    Interact with 750words.com from the command line.
    
    positional arguments:
      FILE              Input files for text to add. Default is to read from
                        standard input.
    
    optional arguments:
      -h, --help        show this help message and exit
      --min MIN         Minimum number of words needed. Default: 750.
      --max MAX         Maximum total number of words allowed. Default: 5000.
      --only-if-needed  Only add text if current word count is below MIN.
      --replace         Replace any current text with the new one, default is to
                        add at the end.
      --count           Don't upload text, only print the current word count.
      --text            Don't upload text, only print the current text.
      --quiet           Don't print progress messages.
    
    debugging options:
      --no-headless     Disable headless mode (opens the Chrome app window).
      --no-quit         Don't quit the browser at the end.
    
    Your 750words.com credentials must be stored in the USER_750WORDS and
    PASS_750WORDS environment variables.

For example (in this case there were already some words entered previously in the day):

    > echo "This is some text to enter" | 750words-client.py
    Got text: This is some text to enter
    
     (6 words)
    Connecting to 750words.com...
    Authenticating...
    Finding current text entry...
    Current word count: 1324
    Entering new text...
    Saving...
    New word count: 1330
    You completed your 750 words for today!
    Done!


<a id="implementation"></a>

### Client Implementation

1.  Dependencies and Dockerfile

    Necessary libraries and software.
    
    -   [Selenium](https://selenium-python.readthedocs.io/) Python bindings (run `pip install -r requirements.txt`). This is the contents of `requirements.txt`:
        
            selenium
    -   [Google Chrome](https://www.google.com/chrome/) is used to automate the connections.
    -   [ChromeDriver](https://chromedriver.chromium.org/) so that Selenium can connect to Chrome - make sure you install the version that corresponds to the Chrome version you have installed.
    
    The Docker image allows the program to be used directly from the container by passing the corresponding arguments, e.g.:
    
        docker run zzamboni/750words-client --help
    
    This is the `Dockerfile` to build it:
    
        ## -*- dockerfile-image-name: "zzamboni/750words-client" -*-
        
        FROM python:3.9-alpine
        MAINTAINER Diego Zamboni <diego@zzamboni.org>
        
        WORKDIR /app
        
        COPY requirements.txt .
        RUN pip install --no-cache-dir -r requirements.txt
        
        RUN apk --no-cache add chromium chromium-chromedriver
        
        COPY 750words-client.py .
        
        ENTRYPOINT [ "python", "/app/750words-client.py" ]

2.  Code

    1.  Libraries
    
        We load the necessary standard libraries.
        
            import argparse
            import os
            import sys
            import time
            import re
        
        We also load the necessary Selenium libraries.
        
            from selenium import webdriver
            from selenium.webdriver.chrome.options import Options
            from selenium.webdriver.common.action_chains import ActionChains
            from selenium.webdriver.common.keys import Keys
            from selenium.webdriver.common.by import By
            from selenium.webdriver.support.ui import WebDriverWait
            from selenium.webdriver.support import expected_conditions as EC
    
    2.  Utility functions
    
        Print a progress/status message to stderr, which can be muted with the `--quiet` option.
        
            def eprint(*eargs, **ekwargs):
                if not args.quiet:
                    print(*eargs, file=sys.stderr, **ekwargs)
        
        Count words in a string. We use simple space-separated word count, which is what 750words.com uses as well.
        
            def word_count(text):
                return len(text.split())
        
        Enter text into a field. We use a Javascript snippet to set the value instead of using the Selenium `send_keys()` function, since it is much faster, particularly for longer texts.
        
            def enter_text(driver, field, value):
                driver.execute_script('arguments[0].value=arguments[1];', field, value)
        
        Find the main text entry field in the page.
        
            def find_text_field(driver):
                return WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, 'entry_body'))
                )
    
    3.  Configuration and command line arguments
    
        We configure the minimum and maximum word thresholds. The maximum may change if you have a paid 750words.com account, which allows you to write more than 5000 words.
        
            min_words = 750
            max_words = 5000
        
        Process the command line options. All the values end up stored in `args`.
        
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
        
        Verify that the username and password have been provided through the corresponding environment variables, otherwise fail.
        
            username = os.getenv('USER_750WORDS') or None
            password = os.getenv('PASS_750WORDS') or None
            
            if not(username and password):
                eprint("Please set the USER_750WORDS/PASS_750WORDS environment variables")
                sys.exit(1)
    
    4.  Read new text
    
        Text is read from the provided files (default STDIN) only if `--count` and `--text` are not given. We also count how many words it contains.
        
            text = ""
            text_count = 0
            if not (args.count or args.text):
                for infile in args.FILE:
                    text = text + infile.read() + "\n"
                text_count = word_count(text)
                eprint("Got text: " + text + (" (%d words)" % text_count))
    
    5.  Start up Chrome using Selenium and connect to 750words.com
    
        Start Chrome using the necessary options. These options ensure that [Chrome runs well inside a Docker container](https://www.intricatecloud.io/2019/05/running-webdriverio-tests-using-headless-chrome-inside-a-container/).
        
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
        
        Now load the website&rsquo;s authentication screen.
        
            eprint("Connecting to 750words.com...")
            driver.get('https://750words.com/auth')
    
    6.  Authenticate
    
        Find the authentication form inside the page.
        
            eprint("Authenticating...")
            login_form = WebDriverWait(driver, 10).until(
                EC.presence_of_element_located((By.ID, 'signin_form'))
            )
        
        If found, find the username/password fields and send the correct information, else signal an error.
        
            if login_form:
                user_field = driver.find_element_by_id('person_email_address')
                password_field = driver.find_element_by_id('person_password')
                enter_text(driver, user_field, username)
                enter_text(driver, password_field, password)
                login_form.submit()
            else:
                raise BaseException("Could not find login form in https://750words.com/auth")
    
    7.  Do the work
    
        By now we should be in the 750words.com main &ldquo;Today&rdquo; page, which contains a big text field for entering today&rsquo;s words. So the first thing we do is find that field.
        
            eprint("Finding current text entry...")
            # We use WebDriverWait to wait (with a limit) until the page is loaded and the
            # necessary element appears.
            # text_field = driver.find_element_by_id('entry_body')
            text_field = find_text_field(driver)
        
        Finally, we can perform the requested actions with the text according to the options.
        
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
                        warning_dialog_text = driver.find_element_by_xpath('//div[@id="losing_words"]').text
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
    
    8.  Finish
    
        We close the driver, which also quits the Chrome instance.
        
            eprint("Done!")
            if not args.no_quit:
                driver.quit()


<a id="emacs-integration"></a>

## Emacs integration

The `750words` Emacs library allows using the `750words-client` command line program to post text from within Emacs. With it, you can post an entire buffer, or a selected region. Support for `auth-sources` is provided so you don&rsquo;t have to store your credentials in your Emacs config.


<a id="emacs-installation"></a>

### Installation

First, you need to have the `750words-client.py` command line installed, or its Docker image.

For now the library is not yet in MELPA, so you need to install it from this repository. If you use Doom Emacs, you can add the following line to your `package.el` file:

    (package! 750words
      :recipe (:host github
               :repo "zzamboni/750words-client"
               :files ("*.el")))

And then load it from your `config.el` as follows:

    (use-package! 750words)

If you prefer to install by hand, you can clone this repository, store the `750words.el` file somewhere in your `load-path`, and load it as follows:

    (require '750words)


<a id="emacs-usage"></a>

### Usage

If you use `auth-sources`, you can store your 750words.com credentials by storing them in the appropriate store associated with the host &ldquo;750words.com&rdquo;. For example, if variable `auth-sources` contains `~/.authinfo.gpg`, you can add a line in the following format:

    machine 750words.com login <email address> password <password>

You can then run `750words-credentials-setenv` to read the credentials and store them in the correct environment variables.

**Note:** If the auth-source you use supports entry creation (for example, `~/.authinfo.gpg` does) you can run `C-u M-x 750words-credentials-setenv` - you will be prompted for your credentials and they will be automatically stored.

After you have loaded your credentials, you can use the following commands to post text:

-   `M-x 750words-region-or-buffer`: if you have a region selected, it will be posted. Otherwise, the whole buffer will be posted.
-   `M-x 750words-region`: post the currently selected region (issues an error if no region is selected).
-   `M-x 750words-buffer`: post the entire current buffer.

By default, the `750words-client.py` is executed, assuming you have it installed. If you want to use its [Docker image](https://hub.docker.com/r/zzamboni/750words-client), you can configure it as follows:

    (setq 750words-client-command "cat %s | docker run -i -e USER_750WORDS -e PASS_750WORDS zzamboni/750words-client")

