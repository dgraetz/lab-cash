# Setting up the low-balance email

The app can email the lab manager when the register runs low. This is optional:
everything else works without it.

There are two parts:

1. **Once, by whoever runs the app:** give the app an email account to send from
   (this page).
2. **Any time, by the lab manager:** turn the email on, choose who gets it and the
   amount that triggers it, in the app's **Lab manager** panel.

## 1. Give the app an email account to send from

We use the lab's Gmail account (e.g. the Dynamic Control account). Gmail doesn't
let apps log in with the normal password, so you make a separate **app password**
just for this app.

1. Sign in to the lab Gmail account and go to <https://myaccount.google.com/security>.
2. Under "How you sign in to Google", turn on **2-Step Verification** if it isn't on
   yet. Google requires it for app passwords.
3. Go to <https://myaccount.google.com/apppasswords>.
4. Type a name like `Lab Cash app` and click **Create**.
5. Google shows a 16-letter password once. Copy it.
6. Open the `.Renviron` file in the app folder and add two lines (no spaces in the password):
   ```
   REGISTER_SMTP_USER=the.lab.account@gmail.com
   REGISTER_SMTP_PASSWORD=abcdefghijklmnop
   ```
7. Restart R (Session → Restart R).

To send from a university account instead of Gmail, ask IT for the SMTP server and
port and add them too:
```
REGISTER_SMTP_HOST=smtp.example.edu
REGISTER_SMTP_PORT=465
```

## 2. Turn it on in the app

1. Open the app, expand **Lab manager**, and enter the PIN.
2. At the bottom, under **Low-balance email**:
   - tick **Email me when the register runs low**
   - enter the address (several addresses are separated by commas)
   - enter the amount, e.g. `50` for "when less than $50 is left"
3. Click **Save email settings**, then **Send a test email** to check that it arrives.
   Check the spam folder the first time.

## How often it emails

One email when a payout takes the balance below the amount. No more emails until
the register has been recounted. After that, the next time it drops low, you get
one email again.

## Publishing to shinyapps.io

Tick `alerts.R` and `.Renviron` when publishing, along with the other files.

## Keep the password safe

The app password can send email as the lab account. It lives only in `.Renviron`,
which is never uploaded to GitHub. If it leaks, delete it at
<https://myaccount.google.com/apppasswords> and make a new one.
