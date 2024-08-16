import test from 'node:test'
import assert from 'node:assert/strict'
import stamped from '../stamped.js'
import simpletime from 'simpletime'

test('getFilename should discovert `Filename` definition', () => {
  const fileStr = '// Filename: test1.js'

  assert.strictEqual(stamped.getFilename(fileStr), 'test1.js')
})

test("should discover a `Filename` definition, with dots", () => {
  const filedotsStr = '// Filename: test1.min.script.js';

  assert.strictEqual(stamped.getFilename(filedotsStr), 'test1.min.script.js')
})

test("should discover a `Filename` definition, with dashes", () => {
  const filedashStr = '// Filename: test1-min-script.js';

  assert.strictEqual(stamped.getFilename(filedashStr), 'test1-min-script.js')
})

test("should return null, if no `Filename` definition", () => {
  assert.strictEqual(stamped.getFilename(''), null)
})


test("should discover an `Author(s)` definition, 1 authors", () => {
  const fileStr = '// Author(s): author1'
  const authors = stamped.getAuthors(fileStr)

  assert.ok(Array.isArray(authors))
  assert.strictEqual(authors[0], 'author1')
});

test("should discover an `Author(s)` definition, 2 authors", () => {
  const fileStr = '// Author(s): author1, author2'
  const authors = stamped.getAuthors(fileStr)

  assert.ok(Array.isArray(authors))
  assert.strictEqual(authors[0], 'author1')
  assert.strictEqual(authors[1], 'author2')
});

test("should return null, if no `Author(s)` definition", () => {
  const authors = stamped.getAuthors('')

  assert.strictEqual(authors, null)
});


const testStrTopArr = [
  '// Timestamp: 2013.02.20-22:41:39 (last modified)  ',
  '// Timestamp: 2013.02.20-22:41:39 (last modified)',
  '// Timestamp: 2013.02.20-22:41:39',
  '// Timestamp: 2013.02.20'
];

// requires date/time methods.... YMDArr.
test("should discover timestamp for `" + testStrTopArr[0] + "`", () => {
  const date_result = stamped.getTimestamp(testStrTopArr[0])
  const date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)');

  date_result.setMilliseconds(0);

  assert.strictEqual(
    date_result.getTime(),
    date_expect.getTime()
  );
});

test("should discover timestamp for `" + testStrTopArr[1] + "`", () => {
  const date_result = stamped.getTimestamp(testStrTopArr[1])
  const date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)')

  date_result.setMilliseconds(0);

  assert.strictEqual(
    date_result.getTime(),
    date_expect.getTime()
  );    
});

test("should discover timestamp for `" + testStrTopArr[2] + "`", () => {
  const date_result = stamped.getTimestamp(testStrTopArr[2])
  const date_expect = new Date('Wed Feb 20 2013 22:41:39 GMT-0800 (PST)')

  date_result.setMilliseconds(0);

  assert.strictEqual(
    date_result.getTime(),
    date_expect.getTime()
  );    
});

test("should discover timestamp for `" + testStrTopArr[3] + "`", () => {
  const date_result = stamped.getTimestamp(testStrTopArr[3])
  const date_expect = new Date('Feb 20 2013 00:00:00 GMT-0800 (PST)')

  date_result.setMilliseconds(0);
  date_result.setSeconds(0);
  date_result.setMinutes(0);
  date_result.setHours(0);

  assert.strictEqual(
    date_result.getTime(),  
    date_expect.getTime()
  );
});

const testStrArr = [
  '// Requires: file1.js, file2.js  ',
  '// Requires: file1.js, file2.js',
  '// Requires: file1.js ',
  '// Requires: file1.js',
  '',
  '// Requires: file1.js,\n' +
    '// file2.js'
]
const displayStrArr = [
  '// Requires: file1.js, file2.js  ',
  '// Requires: file1.js, file2.js',
  '// Requires: file1.js ',
  '// Requires: file1.js',
  '',
  '// Requires: file1.js,\\n// file2.js'
]

test("should discover dependencies for `" + displayStrArr[0] + "`", () => {
  var result = stamped.getDependencies(testStrArr[0]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.js' );    
  assert.strictEqual( result[1], 'file2.js' );    
});

test("should discover dependencies for filenames with dots in them", () => {
  var result = stamped.getDependencies(
    '// Requires: file1.min.script.js, file2.min.script.js  '
  );

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.min.script.js' );    
});

test("should discover dependencies for filenames with dash in them", () => {
  var result = stamped.getDependencies(
    '// Requires: file1-min-script.js, file2-min-script.js  '
  );

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1-min-script.js' );    
});

test("should discover dependencies for filenames that begin on line _after_ `Requires:`", () => {
  var result = stamped.getDependencies(
    '// Requires: \n' +
      '// file1.js,\n' +
      '// file2.js'
  );

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.js' );    
});

test("should discover dependencies for `" + displayStrArr[1] + "`", () => {
  var result = stamped.getDependencies(testStrArr[1]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.js' );    
});

test("should discover dependencies for `" + displayStrArr[2] + "`", () => {
  var result = stamped.getDependencies(testStrArr[2]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.js' );    
});

test("should discover dependencies for `" + displayStrArr[3] + "`", () => {
  var result = stamped.getDependencies(testStrArr[3]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result[0], 'file1.js' );    
});

test("should not discover dependencies for `" + displayStrArr[4] + "`", () => {
  var result = stamped.getDependencies(testStrArr[4]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result.length, 0 );    
});

test("should discover dependencies for `" + displayStrArr[5] + "`", () => {
  var result = stamped.getDependencies(testStrArr[5]);

  assert.ok(Array.isArray(result))
  assert.strictEqual( result.length, 2 );    
});
