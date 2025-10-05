#!/usr/bin/env python3
import os
import secrets
import string
import subprocess
import smtplib
from email.message import EmailMessage
from dotenv import load_dotenv
load_dotenv(dotenv_path="/srv/jupyterhub/.env")  # mount .env into Hub container

# -----------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------

SMTP_SERVER = os.getenv("SMTP_SERVER", "smtp.yourdomain.com")
SMTP_PORT = int(os.getenv("SMTP_PORT", 587))
SMTP_USER = os.getenv("SMTP_USER", "noreply@yourdomain.com")
SMTP_PASS = os.getenv("SMTP_PASS", "")
FROM_ADDR = os.getenv("FROM_ADDR", "JupyterHub Admin <noreply@yourdomain.com>")

# -----------------------------------------------------
# FUNCTIONS
# -----------------------------------------------------

def generate_password(length=16):
    """Generate a secure random password."""
    alphabet = string.ascii_letters + string.digits + "!@#$%^&*()-_"
    return "".join(secrets.choice(alphabet) for _ in range(length))

def set_password(username, password):
    """Update user's PAM password."""
    cmd = ["chpasswd"]
    input_data = f"{username}:{password}\n"
    subprocess.run(cmd, input=input_data.encode(), check=True)

def send_email(to_addr, username, password):
    """Send the new password via SMTP."""
    msg = EmailMessage()
    msg["Subject"] = f"[JupyterHub] New password for {username}"
    msg["From"] = FROM_ADDR
    msg["To"] = to_addr
    msg.set_content(f"""
Hello {username},

Your JupyterHub password has been reset.

Username: {username}
New password: {password}

Please log in and change it immediately after signing in.
If you did not request this reset, contact the administrator.

--
This is an automated message.
""")

    with smtplib.SMTP(SMTP_SERVER, SMTP_PORT) as s:
        s.starttls()
        if SMTP_USER and SMTP_PASS:
            s.login(SMTP_USER, SMTP_PASS)
        s.send_message(msg)

def reset_user(username, email):
    """Generate new password, set it, and email it."""
    new_pass = generate_password()
    set_password(username, new_pass)
    send_email(email, username, new_pass)
    print(f"[INFO] Password for {username} reset and sent to {email}")

# -----------------------------------------------------
# MAIN ENTRY
# -----------------------------------------------------

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Reset JupyterHub PAM user password and email it.")
    parser.add_argument("username", help="System username")
    parser.add_argument("email", help="Recipient email address")
    args = parser.parse_args()

    try:
        reset_user(args.username, args.email)
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] Failed to update password for {args.username}: {e}")
        exit(1)
    except Exception as e:
        print(f"[ERROR] {e}")
        exit(1)
