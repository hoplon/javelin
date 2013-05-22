function info(msg, type) {
  var red, green, reset;
  red   = '\u001b[31m';
  green = '\u001b[32m';
  reset = '\u001b[0m';
  if(type == 'error') {
    console.error(red + msg + reset);
  } else {
    console.log(green + msg + reset);
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

page.open("test/test.html");
