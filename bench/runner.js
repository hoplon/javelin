
function info(msg, type) {
  var system, red, green, reset;
  system  = require('system');
  red     = '\u001b[31m';
  green   = '\u001b[32m';
  reset   = '\u001b[0m';
  if (! msg.length) return;
  if(type == 'error') {
    system.stdout.write(red + msg + reset + "\n");
  } else {
    system.stdout.write(green + msg + reset + "\n");
  }
}

page = new WebPage();

page.onConsoleMessage = info;

page.onError = function(msg, trace) {
  var msgStack = ['ERROR: ' + msg];
  if (trace) {
    msgStack.push('TRACE:');
    trace.forEach(function(t) {
      msgStack.push(' -> ' + t.file + ': ' + t.line + (t.function ? ' (in function "' + t.function + '")' : ''));
    });
  }
  info(msgStack.join('\n'), 'error');
  phantom.exit(1);
};

page.onLoadFinished = function(status) {
  info("All tests passed (on phantomjs).");
  phantom.exit(0);
};

page.open("bench/bench.html");
