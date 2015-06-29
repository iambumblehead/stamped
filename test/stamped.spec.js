var stamped = require('../stamped'),
    simpletime = require('simpletime');

describe('stamped.getFilename', function () {
  var fileStr = '// Filename: test1.js';
  var filedotsStr = '// Filename: test1.min.script.js';
  var filedashStr = '// Filename: test1-min-script.js';

  it("should discover a `Filename` definition", function () {
    expect( stamped.getFilename(fileStr) ).toBe( 'test1.js' );
  });

  it("should discover a `Filename` definition, with dots", function () {
    expect( stamped.getFilename(filedotsStr) ).toBe( 'test1.min.script.js' );
  });

  it("should discover a `Filename` definition, with dashes", function () {
    expect( stamped.getFilename(filedashStr) ).toBe( 'test1-min-script.js' );
  });

  it("should return null, if no `Filename` definition", function () {
    expect( stamped.getFilename('') ).toBe( null );
  });
});

describe("stamped.getAuthors", function () {
  var fileStr, authors;

  it("should discover an `Author(s)` definition, 1 authors", function () {
    fileStr = '// Author(s): author1';
    authors = stamped.getAuthors(fileStr);
    expect( Array.isArray(authors) ).toBe( true );
    expect( authors[0] ).toBe( 'author1' );
  });

  it("should discover an `Author(s)` definition, 2 authors", function () {
    fileStr = '// Author(s): author1, author2';
    authors = stamped.getAuthors(fileStr);
    expect( Array.isArray(authors) ).toBe( true );
    expect( authors[0] ).toBe( 'author1' );
    expect( authors[1] ).toBe( 'author2' );
  });

  it("should return null, if no `Author(s)` definition", function () {
    authors = stamped.getAuthors('');
    expect( authors ).toBe( null );
  });
});

describe("stamped.getTimestamp", function () {
  var testStrArr = [
    '// Timestamp: 2013.02.20-22:41:39 (last modified)  ',
    '// Timestamp: 2013.02.20-22:41:39 (last modified)',
    '// Timestamp: 2013.02.20-22:41:39',
    '// Timestamp: 2013.02.20'
  ];

  // requires date/time methods.... YMDArr.
  it("should discover timestamp for `" + testStrArr[0] + "`", function () {
    var date_result = stamped.getTimestamp(testStrArr[0]),
        date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)');

    date_result.setMilliseconds(0);

    expect(
      date_result.getTime()
    ).toBe(
      date_expect.getTime()
    );
  });

  it("should discover timestamp for `" + testStrArr[1] + "`", function () {
    var date_result = stamped.getTimestamp(testStrArr[1]),
        date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)');

    date_result.setMilliseconds(0);

    expect(
      date_result.getTime()
    ).toBe(
      date_expect.getTime()
    );    
  });

  it("should discover timestamp for `" + testStrArr[2] + "`", function () {
    var date_result = stamped.getTimestamp(testStrArr[2]),
        date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)');

    date_result.setMilliseconds(0);

    expect(
      date_result.getTime()
    ).toBe(
      date_expect.getTime()
    );    
  });

  it("should discover timestamp for `" + testStrArr[3] + "`", function () {
    var date_result = stamped.getTimestamp(testStrArr[3]),
        date_expect = new Date('Feb 20 2013 00:00:00 GMT-0800 (PST)');

    date_result.setMilliseconds(0);
    date_result.setSeconds(0);
    date_result.setMinutes(0);
    date_result.setHours(0);

    expect(
      date_result.getTime()
    ).toBe(
      date_expect.getTime()
    );
  });
});

describe("stamped.getDependencies", function () {
  var testStrArr = [
    '// Requires: file1.js, file2.js  ',
    '// Requires: file1.js, file2.js',
    '// Requires: file1.js ',
    '// Requires: file1.js',
    '',
    '// Requires: file1.js,\n' +
    '// file2.js'
  ];
  var displayStrArr = [
    '// Requires: file1.js, file2.js  ',
    '// Requires: file1.js, file2.js',
    '// Requires: file1.js ',
    '// Requires: file1.js',
    '',
    '// Requires: file1.js,\\n// file2.js'
  ];

  it("should discover dependencies for `" + displayStrArr[0] + "`", function () {
    var result = stamped.getDependencies(testStrArr[0]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.js' );    
    expect( result[1] ).toBe( 'file2.js' );    
  });

  it("should discover dependencies for filenames with dots in them", function () {
    var result = stamped.getDependencies(
      '// Requires: file1.min.script.js, file2.min.script.js  '
    );

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.min.script.js' );    
  });

  it("should discover dependencies for filenames with dash in them", function () {
    var result = stamped.getDependencies(
      '// Requires: file1-min-script.js, file2-min-script.js  '
    );

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1-min-script.js' );    
  });

  it("should discover dependencies for filenames that begin on line _after_ `Requires:`", function () {
    var result = stamped.getDependencies(
      '// Requires: \n' +
      '// file1.js,\n' +
      '// file2.js'
    );

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.js' );    
  });

  it("should discover dependencies for `" + displayStrArr[1] + "`", function () {
    var result = stamped.getDependencies(testStrArr[1]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.js' );    
  });

  it("should discover dependencies for `" + displayStrArr[2] + "`", function () {
    var result = stamped.getDependencies(testStrArr[2]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.js' );    
  });

  it("should discover dependencies for `" + displayStrArr[3] + "`", function () {
    var result = stamped.getDependencies(testStrArr[3]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result[0] ).toBe( 'file1.js' );    
  });

  it("should not discover dependencies for `" + displayStrArr[4] + "`", function () {
    var result = stamped.getDependencies(testStrArr[4]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result.length ).toBe( 0 );    
  });

  it("should discover dependencies for `" + displayStrArr[5] + "`", function () {
    var result = stamped.getDependencies(testStrArr[5]);

    expect( Array.isArray(result) ).toBe( true );    
    expect( result.length ).toBe( 2 );    
  });
});

