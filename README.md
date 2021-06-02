A simple client for posting text to your [750words.com](https://750words.com/) account. Can also be used to query your current word count or the current text.

You can see its annotated source code in the [750words-client.org](https://github.com/zzamboni/750words-client/blob/main/750words-client.org) file, but the file you run is [750words-client.py](https://github.com/zzamboni/750words-client/blob/main/750words-client.py).


# Table of Contents

1.  [Installation](#installation)
    1.  [Docker image](#docker-image)
    2.  [Local installation](#local-installation)
2.  [Usage](#usage)


<a id="installation"></a>

# Installation


<a id="docker-image"></a>

## Docker image

[[750words-client on Docker Hub](https://hub.docker.com/r/zzamboni/750words-client)][[Dockerfile](https://github.com/zzamboni/750words-client/blob/main/Dockerfile)]

You can use `750words-client.py` from its Docker image as follows (the image will be downloaded from [Docker Hub](https://hub.docker.com/r/zzamboni/750words-client) the first time you use it):

    docker run zzamboni/750words-client --help

Note: you need to pass the `-i` option to `docker run` if you want to read the input from standard input, e.g.:

    cat file.txt | docker run -i zzamboni/750words-client

If you want to build the image yourself, you can do it as follows from a checkout of its [git repository](https://github.com/zzamboni/750words-client):

    docker build --tag 750words-client .


<a id="local-installation"></a>

## Local installation

Clone this repository:

    git clone https://github.com/zzamboni/750words-client.git

You need the following libraries and components installed:

-   [Selenium](https://selenium-python.readthedocs.io/) Python bindings (run `pip install -r requirements.txt`)
-   [Google Chrome](https://www.google.com/chrome/) is used to automate the connections.
-   [ChromeDriver](https://chromedriver.chromium.org/) so that Selenium can connect to Chrome - make sure you install the version that corresponds to the Chrome version you have installed.

You can then copy `750words.py` to somewhere in your `$PATH` to use it.


<a id="usage"></a>

# Usage

    750words-client.py --help

    usage: 750words-client.py [-h] [--min MIN] [--max MAX] [--only-if-needed]
                              [--replace] [--count] [--text] [--no-headless]
                              [--quiet]
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
      --no-headless     Disable headless mode (opens the Chrome app window).
      --quiet           Don't print progress messages.
    
    Your 750words.com credentials must be stored in the USER_750WORDS and
    PASS_750WORDS environment variables.

