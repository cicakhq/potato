/* Fonts related tasks */
var gulp       = require('gulp');
var flatten    = require('gulp-flatten');
var plumber    = require("gulp-plumber");

var basePath   = global.basePath;

gulp.task('fonts:awe', function () {
  gulp.
    src([ basePath.bower+'font-awesome/fonts/*.{ttf,woff,eof,svg}' ]).
    pipe(plumber()).
    pipe(flatten()).
    pipe(gulp.dest(basePath.dest+'fonts/awe'));
});

gulp.task('fonts:ssp', function () {
  var source      = basePath.bower+'fontface-source-sans-pro/fonts/';

  gulp.
    src([ source + '**/*.{ttf,woff,eof,svg}' ]).
    pipe(plumber()).
    pipe(flatten()).
    pipe(gulp.dest(basePath.dest+'fonts/ssp'));
});

gulp.task('fonts:symb', function() {
    var source = basePath.src + 'fonts/symb/';
    gulp.
        src([ source + '**/*.{ttf,woff,eot}' ]).
        pipe(plumber()).
        pipe(flatten()).
        pipe(gulp.dest(basePath.dest+'fonts/symb'));
});

gulp.task('fonts:noto', function() {
    var source = basePath.src + 'fonts/Noto/';
    gulp.
        src([ source + '**/*.{ttf,woff,eot}' ]).
        pipe(plumber()).
        pipe(flatten()).
        pipe(gulp.dest(basePath.dest+'fonts/Noto'));
});

gulp.task('fonts:package', [ 'fonts:ssp', 'fonts:awe', 'fonts:symb', 'fonts:noto' ]);
