# potato

Delicious conversations platform in Common Lisp and ClojureScript.

# Features

  - Markdown-like syntax for messages
  - File uploads to S3 or local storage with image thumbnails in the channel
  - Inline maths based on MathJax
  - Detection of URL's with automatic insertion of thumbnails or description
    with built-in support for Wikipedia, Youtube, Github and XKCD. It is easy
    to add support for other sites
  - Email notifications sent to users after being mentioned or when they receive
    private messages if the user didn't read the messages
  - Group-based permission system (the user interface for this is not complete yet)
  - Message search based on Apache Solr
  - Private messages are implemented as a special channel, giving the private chats
    identical functionality as the main channels
  - API that allows for writing external clients

# How to install

A manual installation guide can be found in `docs/INSTALL.md`. We're also working on a more
streamlined installation that does not require as many manual steps.

Help from others is always appreciated of course.

# Demo installation

Demo installation at: http://potato.dhsdevelopments.com/

No guarantees are made as to the persistence of anything posted on
that server.
