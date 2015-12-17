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
    bower: 'libs/',
    tmpl:  '../src/template/manifest/'
};

requireDir('./gulp');

gulp.task('build', [
    'fonts:package',
    'img:package',
    'vendor:package',
    'html:compile',
    'css:compile',
    'js:lint',
    'js:compile'
]);

gulp.task('watch', [
    'css:watch',
    'js:watch',
    'html:watch'
]);

gulp.task('default', [
    'build',
    'watch'
]);

