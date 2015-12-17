/* CSS (and LESS) related tasks */
var argv       = require('yargs').argv;
var gulp       = require('gulp');
var rev        = require('gulp-rev');
var sourcemaps = require('gulp-sourcemaps');
var less       = require('gulp-less');
var prefixer   = require('gulp-autoprefixer');
var minifycss  = require('gulp-minify-css');
var gulpif     = require('gulp-if');
var notify     = require('gulp-notify');
var size       = require('gulp-size');
var plumber    = require("gulp-plumber");
var buffer     = require('gulp-buffer');

var isProd     = !!(argv.production);
var isDev      = !isProd;

var basePath   = global.basePath;

gulp.task('css:watch', function () {
    gulp.watch(basePath.src+'css/**/*', ['css:compile']);
});

/* supports plain CSS and LESS format */
gulp.task('css:compile', function () {
    gulp.src(basePath.src+'css/*', { nodir: true })
    .pipe(plumber())
    //.pipe(sourcemaps.init())
    .pipe(gulpif(/[.]less$/, less()))
    //.pipe(sourcemaps.write())
    .pipe(prefixer({
        browsers: ['last 4 versions'],
        cascade: false
    }))
    .pipe(gulpif(isProd,minifycss()))
    .pipe(buffer())
    .pipe(gulpif(
        isProd,size({
            title: 'CSS production',
            gzip: true,
            showFiles: false
        }), size({  // else clause
            title: 'CSS development',
            showFiles: true
        })))
    .pipe(rev())
    .pipe(gulp.dest(basePath.dest+'css'))
    .pipe(notify({
        title:   "Potato's Gulp",
        message: "CSS compiled for "+(isProd? "production":"development"),
        onLast:  true
    }))
    .pipe(rev.manifest('css.manifest'))
    .pipe(gulp.dest(basePath.tmpl));
});
