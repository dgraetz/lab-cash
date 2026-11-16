// Lab Cash email sender (Google Apps Script)
//
// This runs on the lab's Google account and sends the low-balance emails for
// the Lab Cash app. The app sends it the address, subject and text, plus a
// secret code. If the code matches, this script emails the message from the
// Google account it belongs to. No password is needed.
//
// A copy of this code lives in the Lab Cash repo as mail_script.gs.
// Setup steps are in EMAIL_SETUP.md.

// Called by the Lab Cash app.
function doPost(e) {
  var secret = PropertiesService.getScriptProperties().getProperty('SECRET');
  var data = JSON.parse(e.postData.contents);

  if (!secret || data.secret !== secret) {
    return reply({ ok: false, error: 'wrong secret' });
  }
  if (data.dry_run) {
    // Only a check: the secret is right and the script can send email
    return reply({ ok: true, dry_run: true, emails_left_today: MailApp.getRemainingDailyQuota() });
  }

  MailApp.sendEmail({ to: data.to, subject: data.subject, body: data.body, name: 'Lab Cash' });
  return reply({ ok: true });
}

// Run this once by hand. It makes the secret code (if there isn't one yet)
// and shows it in the log, so it can be copied into the app's .Renviron.
function setup() {
  var props = PropertiesService.getScriptProperties();
  if (!props.getProperty('SECRET')) {
    props.setProperty('SECRET', Utilities.getUuid().replace(/-/g, '') + Utilities.getUuid().replace(/-/g, ''));
  }
  Logger.log('Emails this account can still send today: ' + MailApp.getRemainingDailyQuota());
  Logger.log('REGISTER_MAIL_SECRET=' + props.getProperty('SECRET'));
}

function reply(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj))
    .setMimeType(ContentService.MimeType.JSON);
}
