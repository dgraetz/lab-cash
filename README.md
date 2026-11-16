# Lab Cash

A small R Shiny app for tracking the cash register in a lab. Research assistants
record the money they pay out, everyone sees what should be left in the register,
and the lab manager recounts the register to reset the balance.

Two versions are in this folder:

| File | What RAs enter |
|---|---|
| `app_bills.R` | How many $20, $10, $5 and $1 bills they took out (+ / − buttons) |
| `app.R` | Just the amount and the date |

## Running it

1. Open `register_tracker.Rproj` in RStudio.
2. Create a file called `.Renviron` in this folder with the manager PIN and,
   if you use a Google Sheet, its ID:
   ```
   REGISTER_MANAGER_PIN=1234
   REGISTER_SHEET_ID=1AbC...xyz
   ```
   Then restart R (Session → Restart R).
3. Open `app_bills.R` (or `app.R`) and click **Run App**.

Needs the packages `shiny`, `bslib`, `googlesheets4` (for Google Sheets) and `httr` (for email).

## Where the data is saved

See `storage.R`. With `REGISTER_SHEET_ID` set in `.Renviron`, data goes to a Google
Sheet, which is needed on shinyapps.io because it doesn't keep files. Without it,
data goes to CSV files in `data/`. Setting up the sheet is described in `GOOGLE_SETUP.md`.

## Low-balance email (optional)

The lab manager can have the app email them when the total or a bill type runs low. Turn it on
in the app's **Lab manager** panel. Emails are sent by a small Google Apps Script
(`mail_script.gs`); see `EMAIL_SETUP.md`. The app code is in `alerts.R`.

`google-key.json` and `.Renviron` hold secrets and are kept out of git by `.gitignore`.
