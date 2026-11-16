# Saving the register data in a Google Sheet

On shinyapps.io the app loses its own files whenever it restarts, so the data
has to be saved somewhere else. These steps connect the app to a Google Sheet.
You do this once, and it takes about 15 minutes.

The app logs in to Google with a **service account**: a robot Google account
that belongs to your project. You share the sheet with it the same way you'd
share it with a person.

## 1. Make the sheet

1. In Google Sheets, create an empty spreadsheet, e.g. "Lab Cash data".
   A lab Google account is better than a personal one.
2. Copy its ID from the address bar. It's the long part between `/d/` and `/edit`:
   `https://docs.google.com/spreadsheets/d/`**`1AbC...xyz`**`/edit`

## 2. Make the service account

1. Go to <https://console.cloud.google.com>, signed in with the same Google account.
2. At the top, create a new project, e.g. "lab-cash".
3. Menu → **APIs & Services → Library**. Search for **Google Sheets API** and click **Enable**.
4. Menu → **IAM & Admin → Service Accounts → Create service account**.
   Give it a name like `lab-cash-app`. You can skip the optional role and access steps.
5. Click the new service account → **Keys** tab → **Add key → Create new key → JSON**.
   A `.json` file downloads.
6. Rename that file to `google-key.json` and move it into this folder, next to `app.R`.
7. Copy the service account's email address. It looks like
   `lab-cash-app@lab-cash-12345.iam.gserviceaccount.com`.

If step 5 says key creation is disabled, your organization (often a university
Google account) blocks it. Use a regular Gmail account for the project instead.

## 3. Share the sheet with the service account

In your sheet, click **Share**, paste the service account email, choose
**Editor**, and untick "Notify people". Click Share.

## 4. Tell the app about the sheet

Open the `.Renviron` file in this folder (create it if it isn't there) and add
the sheet ID on its own line:

```
REGISTER_SHEET_ID=1AbC...xyz
```

Restart R (Session → Restart R) so it reads the file, then run the app in RStudio. It creates the tabs it needs (`bills_payouts`,
`bills_counts`, or `payouts`, `counts` for the simple version). Record a test
payout and check that it shows up in the sheet. Then delete the test row in
the app.

## 5. Publish

Click **Publish** in RStudio. In the list of files to upload, make sure these are ticked:

- `app_bills.R` (or `app.R`)
- `storage.R`
- `alerts.R`
- `idle.R`
- `www/logo.svg`
- `google-key.json`
- `.Renviron` (holds the sheet ID and the manager PIN)

## Keep the key safe

`google-key.json` works like a password for the sheet. Anyone who has the file
can read and change the data. Don't email it, put it on a shared drive, or
upload it to GitHub. If it leaks, delete the key in the Google Cloud console
(step 2.5) and make a new one.
