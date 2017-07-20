/*
 * gulpfile.js
 * ===========
 * Rather than manage one giant configuration file responsible
 * for creating multiple tasks, each task has been broken out into
 * its own file in gulp/tasks. Any files in that directory get
 * automatically required below.
 *
 */

var requireDir = require('require-dir');
var os         = require('os');
var gulp       = require('gulp');

global.basePath = {
    src:   'src/',
    dest:  '../public/assets/',
    bower: 'libs/'
};

requireDir('./gulp');

gulp.task('build', [
    'fonts:package',
    'vendor:package'
]);

gulp.task('default', [
    'build'
]);
