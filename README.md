# gmail-send
Command line interface for sending Gmail emails

## Prerequisites

* [Python 3](https://python.org)

## Usage

```
usage: gmail-send.py [-h] [-s SUBJECT] [-v] [--html]
                     gmail_user gmail_pwd recipient

Send an email message via SMTP. Message body is read from stdin.

positional arguments:
  gmail_user            sender's gmail username (without domain)
  gmail_pwd             sender's gmail password
  recipient             recipient's email address (with domain)

optional arguments:
  -h, --help            show this help message and exit
  -s SUBJECT, --subject SUBJECT
                        email subject
  -v, --verbose         output sent message
  --html                send email as HTML
```

## Example

```sh
$ echo 'Hello World!' | ./gmail-send.py -s 'Howdy!' john.smith $(cat john.smith.passwd) max.mustermann@gmail.com
```
