# Low-balance emails

The app can email the lab manager when the register runs low. This is optional:
everything else works without it.

## Choosing when to get an email

In the app, expand the **Lab manager** panel, enter the PIN, and go to
**Low-balance emails** at the bottom:

1. Tick **Send me an email when the register runs low**.
2. Enter the address. Separate several addresses with commas.
3. Fill in the limits you want and leave the others empty:
   - **Email when the total drops below** a dollar amount, e.g. `100`.
   - In the bills version, also **per bill type**: e.g. `20` under **$1 bills**
     means "email me when 20 or fewer $1 bills are left".
4. Click **Save email settings**, then **Send a test email** to check that it arrives.
   Check the spam folder the first time.

Each limit triggers one email. If a payout breaks several limits at once, they
come together in one email. A limit isn't emailed about again until the register
has been recounted.

## How the emails are sent

The app doesn't log in to an email account, so no password or 2-Step Verification
is needed. A small Google Apps Script called **Lab Cash email**, on the Dynamic
Control Google account, sends the emails from that account. Its code is in
`mail_script.gs`.

The app reaches the script through two lines in `.Renviron`:

```
REGISTER_MAIL_URL=https://script.google.com/macros/s/.../exec
REGISTER_MAIL_SECRET=...
```

The secret code makes sure only this app can use the script. Google allows the
account to send 100 of these emails a day.

These are already set up. The steps below are only needed to set it up again,
for example on a different Google account.

### Setting it up from scratch

1. Signed in to the Google account that should send the emails, go to
   <https://script.google.com> and create a **New project**. Name it "Lab Cash email".
2. Replace the code with the contents of `mail_script.gs` and save.
3. Choose `setup` in the function list and click **Run**. Google asks for
   permission. Click **Review permissions**, pick the account, then
   **Advanced → Go to Lab Cash email (unsafe)** (that's normal for your own
   scripts), then **Allow**.
4. The log at the bottom shows a line `REGISTER_MAIL_SECRET=...`. Copy that line
   into `.Renviron`.
5. Click **Deploy → New deployment**, choose type **Web app**, set
   **Execute as: Me** and **Who has access: Anyone**, and click **Deploy**.
   Authorize again if asked.
6. Copy the web app URL and add it to `.Renviron` as `REGISTER_MAIL_URL=...`.
7. Restart R.

## Publishing to shinyapps.io

Tick `alerts.R` and `.Renviron` when publishing, along with the other files.

## Keep the secret safe

Anyone who has both the URL and the secret can send emails from the lab account.
They only live in `.Renviron`, which is never uploaded to GitHub. If they leak,
delete the `SECRET` entry in the script's **Project Settings → Script
properties**, run `setup` again, and put the new secret in `.Renviron`.
