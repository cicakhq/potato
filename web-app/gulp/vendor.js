/* Vendor JS directory */
var gulp       = require('gulp');
var plumber    = require("gulp-plumber");
var notify     = require('gulp-notify');
var flatten    = require('gulp-flatten');

var basePath   = global.basePath;

gulp.task('vendor:src', function () {
   return gulp.src([ 'src/vendor/**/*' ])
           .pipe(gulp.dest('../public/assets/vendor'));
});

gulp.task('vendor:contextmenu', function () {
  gulp.
    src([ basePath.bower+'jQuery-contextMenu/src/**/*.{js,css,png}' ]).
    pipe(plumber()).
    pipe(flatten()).
    pipe(gulp.dest(basePath.dest+'vendor/context'));
});

gulp.task('vendor:dropit', function () {
  gulp.
    src([ basePath.bower + 'dropit/dropit.js' ]).
    pipe(plumber()).
    pipe(gulp.dest(basePath.dest + 'vendor'));
});


gulp.task('vendor:datatables', function () {
  gulp.
    src([ basePath.bower + 'datatables/media/js/jquery.dataTables.min.js' ]).
    pipe(plumber()).
    pipe(gulp.dest(basePath.dest + 'vendor'));
});

gulp.task('vendor:jquery', function () {
  gulp.
    src([ basePath.bower + 'jquery/dist/jquery.min.js' ]).
    pipe(plumber()).
    pipe(gulp.dest(basePath.dest + 'vendor'));
});

gulp.task('vendor:package',
          [ 'vendor:src', 'vendor:contextmenu',
            'vendor:dropit', 'vendor:datatables',
            'vendor:jquery' ]);
