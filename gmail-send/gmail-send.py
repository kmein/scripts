#!/usr/bin/env python3

import sys

def send_email(user, pwd, recipient, subject, body, content_type):
    import smtplib
    from email.mime.text import MIMEText

    gmail_user = user
    gmail_pwd = pwd

    FROM = user
    TO = recipient if type(recipient) is list else [recipient]
    SUBJECT = subject
    TEXT = body

    # Prepare actual message
    message = MIMEText(TEXT, content_type, "utf-8")
    message["From"] = FROM
    message["To"] = ", ".join(TO)
    message["Subject"] = SUBJECT
    message_string = message.as_string()

    try:
        server = smtplib.SMTP("smtp.gmail.com", 587)
        server.ehlo()
        server.starttls()
        server.login(gmail_user, gmail_pwd)
        server.sendmail(FROM, TO, message_string)
        server.close()
        return message_string
    except:
        print("Failed to send mail: ", sys.exc_info())

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Send an email message via SMTP. Message body is read from stdin."
    )
    parser.add_argument("gmail_user", help="sender's gmail username (without domain)")
    parser.add_argument("gmail_pwd", help="sender's gmail password")
    parser.add_argument("recipient", help="recipient's email address (with domain)")
    parser.add_argument("-s", "--subject", help="email subject")
    parser.add_argument("-v", "--verbose", help="output sent message", action="store_true")
    parser.add_argument("--html", help="send email as HTML", action="store_true")
    args = parser.parse_args()

    body = sys.stdin.read()
    msg = send_email(
        args.gmail_user,
        args.gmail_pwd,
        args.recipient,
        args.subject if args.subject is not None else "",
        body,
        "html" if args.html else "plain"
    )
    if args.verbose:
        print("\n%s" % msg)

