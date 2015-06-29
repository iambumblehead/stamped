// Filename: stamped.js  
// Timestamp: 2015.06.29-13:46:50 (last modified)
// Copyright: Ganimas LLC  
// Author(s): bumblehead <chris@bumblehead.com>  

var path = require('path'),
    simpletime = require('simpletime');

var stamped = module.exports = (function (o) {

  // `Filename` must be defined at the top of the file  
  // 
  // get filename as it is defined in the file,
  // ex., `Filename: Main.js` === "Main.js"
  o.getFilename = function (filestr) {
    var regex = /Filename: (.*\.(js|css|less))\b/m,
        match = filestr.match(/^.*$/m)[0], // first line
        fileName = null;

    if (match && (match = match.match(regex))) {
      fileName = match[1];
      fileName = path.basename(fileName);
    }

    return fileName;
  };

  // get array of authors as they are defined in the file,
  // ex., `Author(s): author1, author2` === ["author1", "author2"]
  o.getAuthors = function (filestr) {
    var regex = /Author(\(s\))?: (.*)[\r\n\b]?/,
        match = filestr.match(regex);

    return match && match[2] &&
      match[2].trim().split(/, ?/);      
  };

  o.getFormattedDate = function (date) {
    return simpletime.applyFormatDate(date, 'yyyy.mm.dd-hh:mm:ss');
  };

  // get timestamp as defined in file. timestamp is returned as date object
  // the following are valid timestamps:
  // `Timestamp: 2013.02.20-22:41:39 (last modified)  `
  // `Timestamp: 2013.02.20-22:41:39 (last modified)`
  // `Timestamp: 2013.02.20-22:41:39`
  // `Timestamp: 2013.02.20`
  o.getTimestamp = function (file) {
    var regex = /Timestamp:\s(\d{4}\.\d\d\.\d\d)(-\d\d:\d\d:\d\d)?\b/,
        match = file.match(regex), m, hh, mm, ss, hhmmssMatch;
    
    m = match ? simpletime.parseISO8601(match[1]) : null;
    if (m && match[2]) {
      hhmmssMatch = match[2].match(/-(\d\d):(\d\d):(\d\d)/);
      hh = hhmmssMatch[1];
      mm = hhmmssMatch[2];
      ss = hhmmssMatch[3];
      
      m.setHours(hh);
      m.setMinutes(mm);
      m.setSeconds(ss);

      return m;
    }
    return m;
  },

  // get dependencyArr as defined in this file. each dependency is a string.
  // multiple dependencies may be defined. the following are valid dependencies:
  // `Requires: file.js`
  // `Requires: file.js  `
  // `Requires: file.js, file2.js`
  // `Requires: file.js,\n file2.js`
  o.getDependencies = function (file) {
    var requiresRe = /Requires: ?[,\r\n ]?((\/\/)? ?[\w.-]*\.[cj]ss?([,\r\b ]?)*(\n\/\/ )?)*/,
        filenameRe = /[\w.-]*\.[cj]ss?/g,
        match = file.match(requiresRe),
        dependencyArr = [];

    match = file.match(requiresRe);
    if (match) {
      match = match[0].match(filenameRe);
      if (match) {
        dependencyArr = match;
      }
    }

    return dependencyArr;
  };
  
  // constructs a the 'head' of a javascript file
  // ex.
  //  // Filename: Main.js
  //  // Timestamp: 2013.04.01-12:08:54 (last modified)
  //  // Author(s): Bumblehead (www.bumblehead.com)
  //  // Copyright: FOX
  //
  o.getjshead = function (spec) {
    var timestamp = simpletime.getDateAsISO(spec.timestamp),
        authorsArr = spec.authorsArr || [],
        headText = '';

    headText +=
      "// Filename: " + spec.filename + "  \n" +
      "// Timestamp: " + timestamp + " (last modified)  \n";

    if (authorsArr.length) {
      headText +=
        "// Author(s): " + authorsArr.join(', ') + "  \n";    
    }

    if (spec.copyright) {
      headText += "//  \n" +
        "// " + spec.copyright + "  \n";        
    }

    headText += '\n';
    
    return headText;
  };

  o.getfilenamere = function (spec) {
    var datePattern = '\\d{4}\\.\\\d{2}\\.\\d{2}',
        timePattern = '\\d{2}:\\d{2}:\\d{2}',
        beginBndry = '[\/\"\']',
        filename = path.basename(spec.filepath),
        fileextn = path.extname(filename),      
        basename = path.basename(filename, fileextn),
        reStr;

    reStr = basename;
    if (spec && spec.isTimestamped) {
      reStr += '_' + datePattern + '-' + timePattern;
    }
    reStr += fileextn;

    return new RegExp(reStr);    
  };
  
  return o;
  
}({}));
