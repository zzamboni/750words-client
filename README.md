

# 750words command-line client

A simple client for posting text to your [750words.com](https://750words.com/) account. Can also be used to query your current word count or the current text. Emacs integration is also available.

You can see its full annotated source code in the [750words-client.org](https://github.com/zzamboni/750words-client/blob/main/750words-client.org) file.


# Table of Contents

1.  [750words command-line client](#org62df48c)
    1.  [Installation](#installation)
        1.  [Docker image](#docker-image)
        2.  [Installation from source](#local-installation)
    2.  [Usage](#usage)
    3.  [Emacs integration](#emacs-integration)
        1.  [Installation](#emacs-installation)
        2.  [Usage](#emacs-usage)


<a id="installation"></a>

## Installation


<a id="docker-image"></a>

### Docker image

[[750words-client on Docker Hub](https://hub.docker.com/r/zzamboni/750words-client)][[Dockerfile](https://github.com/zzamboni/750words-client/blob/main/Dockerfile)]

You can use `750words-client.py` from its Docker image as follows (the image will be downloaded from [Docker Hub](https://hub.docker.com/r/zzamboni/750words-client) the first time you use it):

    docker run zzamboni/750words-client --help

Note that you have to define the `USER_750WORDS` and `PASS_750WORDS` environment variables in your environment, and pass them to the container. You also need to pass the `-i` option to `docker run` if you want to read the input from standard input, e.

    cat file.txt | docker run -i -e USER_750WORDS -e PASS_750WORDS zzamboni/750words-client

If you want to build the image yourself, you can do it as follows from a checkout of its [git repository](https://github.com/zzamboni/750words-client):

    docker build --tag 750words-client .


<a id="local-installation"></a>

### Installation from source

[[GitHub repository](https://github.com/zzamboni/750words-client)]

Clone the git repository:

    git clone https://github.com/zzamboni/750words.git

You need the following libraries and components installed:

-   [Selenium](https://selenium-python.readthedocs.io/) Python bindings (run `pip install -r requirements.txt`)
-   [Google Chrome](https://www.google.com/chrome/) is used to automate the connections.
-   [ChromeDriver](https://chromedriver.chromium.org/) so that Selenium can connect to Chrome - make sure you install the version that corresponds to the Chrome version you have installed.

You can then copy `750words-client.py` to somewhere in your `$PATH` to use it.


<a id="usage"></a>

## Usage

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

