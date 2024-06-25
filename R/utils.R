# librería de mis funciones útiles de R

#' Print a console tree
#'
#' @param root
#' A tree, a data structure like a nested list with one "root", that is, a
#' single node with possibly multiple subnodes each with subnodes recursively.
#' Each node has a "label" string to be printed.
#' @param nodes
#' Function that extract list of nodes from each node. By default, nodes are
#' the "nodes" element of a list.
#' @param label
#' Function that extract label from each node. By default, stored in "label"
#' element of a list.
#' @return
#' NULL invisibly
#' @export
#' @examples
#' tree.rpois <- function(lambda = 2, decr = 0.75) {
#'   cnt <- 0
#'   .f <- function(i, p) {
#'     cnt <<- cnt + 1
#'     list(label = sQuote(cnt),
#'          nodes = lapply(seq_len(rpois(1, p)), .f, p * decr))
#'   }
#'   .f(1, lambda)
#' }
#' set.seed(847)
#' tree <- tree.rpois()
#' displayTree(tree)
#'
displayTree <- function(root, nodes = \(x) x$nodes, label = \(x) x$label) {
  sp <- "  "
  mt <- intToUtf8(c(9474, 32))     # │
  mb <- intToUtf8(c(9500, 9472))   # ├─
  fb <- intToUtf8(c(9492, 9472))   # └─
  .branch <- function(node, br = logical(0)) {
    cat(ifelse(br[length(br)], fb, mb), label(node), "\n", sep = "")
    for (i in seq(n <- nodes(node))) {
      cat(ifelse(br, sp, mt), sep = "")
      .branch(n[[i]], c(br, i == length(n)))
    }
  }
  .branch(root)
  invisible()
}


#' let, letrec, let*, letrec*
#' @description Utilities inspired in lisp/scheme let and letrec
#' Allow evaluating an expression in an environment with custom bounded
#' variables. Similar to \code{with} and \code{local} but variables are
#' passed individually in ... instead of lists or code blocks. Specially useful
#' with functions.
#' @param expr Expression to be evaluated in custom environment
#' @param ... named variables or functions which will be bounded in the
#' evaluation environment.
#' @param parent Optionally a parent environment.
#' @return
#' Value of the evaluated expression.
#' @details
#' \code{let} will evaluate the expressions given in ... in the current
#' environment or the environment given in parent, while \code{letrec} will
#' evaluate the dots in an environment in which the variables are bound. This
#' means that functions passed in ... to \code{let} cannot see themselves, which
#' is useful to allow them to call functions in the parent (the current)
#' environment with the same name from inside the functions, potentially adding
#' additional functionality.
#' On the other hand, \code{letrec} can see and recursively call all values
#' passed in ..., this is the same as \code{local()}, but with each function
#' passed individually as an argument instead of a code block.
#' \code{let..} and \code{letrec..} are variants much alike scheme's let\* and
#' letrec*, in which each argument can see the previous only. They are
#' implemented as recursive nested calls, i.e. \code{let..(<expr>, a, b, ...)}
#' is the same as \code{... let(let(expr, a), b), ... }
#'
#' @export
#'
#' @examples
#' # let allow you to refer to an outside function with the same name without
#' # recursion:
#' f <- function(x) x ^ 2 - 5
#' f(2)  # -1
#' let(f(2), f = function(x) abs(f(x)))  # 1
#'
#' # but this is an error as "let" functions cannot see themselves
#' try(let(
#'   k(10),
#'   k = function(y) f(y),
#'   f = function(x) if(x) x*k(x-1) else 1))
#'
#' # letrec functions are recursive:
#' letrec(
#'   k(10),
#'   k = function(y) f(y),
#'   f = function(x) if(x) x*k(x-1) else 1)
#'
#' # let.. can see previously defined arguments in order:
#' let..(g(2),
#'   f = function(x) x ^ 2 - 5,
#'   g = function(x) abs(f(x)))  # 1
#'
let <- function(expr, ..., parent = parent.frame()) {
  env <- evalq(environment(), parent, parent.frame())
  eval(substitute(expr), lapply(substitute(...()), eval, env), env)
}

#' @rdname let
#' @export
letrec <- function(expr, ..., parent = parent.frame()) {
  env <- new.env(parent = evalq(environment(), parent, parent.frame()))
  eval(substitute(expr), list2env(lapply(substitute(...()), eval, env), env))
}

#' @rdname let
#' @export
let.. <- function(expr, ..., parent = parent.frame()) {
  env <- evalq(environment(), parent, parent.frame())
  nest <- function(cl, dots, i) {
    if (i == 0) cl
    else nest(as.call(c(quote(let), cl, dots[i])), dots, i - 1)
  }
  eval(nest(substitute(expr), substitute(...()), ...length()), env)
}

#' @rdname let
#' @export
letrec.. <- function(expr, ..., parent = parent.frame()) {
  env <- evalq(environment(), parent, parent.frame())
  nest <- function(cl, dots, i)  {
    if (i == 0) cl
    else nest(as.call(c(quote(letrec), cl, dots[i])), dots, i - 1)
  }
  eval(nest(substitute(expr), substitute(...()), ...length()), env)
}


#' Begin a code block
#'
#' @description An alias to \code{{}} braces to allow multiple statements to
#' be expressed in a single call.
#'
#' @usage begin(...)
#' @param ... any number of comma-separated R statements
#'
#' @export
#' @return Last evaluated statement or NULL.
#'
#' @examples
#' begin(a <- 1, a + 1)  # 2
begin <- .Primitive("{")

#' Calculate number in another base (radix)
#' @description Make a representation of a numeric vector on another,
#' possibly non integer, numeric base (radix)
#'
#' @param n
#' numeric vector to change base, or the result of a previous call to rebase.
#' @param base
#' base to change representation to. If ommited and n is a result of rebase,
#' the return value is the numeric vector.
#' @param prec precision, in number of digits
#'
#' @return
#' for a numeric vector, an object of class \emph{rebase} (a matrix)
#' for an object of class \emph{rebase}, if \emph{rebase} is ommited return
#' a vector, otherwise a new object of class \emph{rebase} on the provided base.
#' @export
#' @details
#' Note that precision may be lost when converting from one base to another
#' and then back.
#'
#' @examples
#' x <- rebase(c(0,100), base = 10)
#' print(x)
#' rebase(x)
#' y <- rebase(c(0.8,90), 2, prec = 10)
#' print(y)
#' rebase(y) # 0.75, 90 (precision loss)
#' fi = (1 + sqrt(5))/2
#' rebase(8788, base=fi, prec=10)
#'
rebase <- function(n, base, prec) {

  stopifnot(inherits(n, "rebase") || !missing(base))
  if (!missing(base))
    stopifnot(is.numeric(base) && length(base) == 1 && base > 1)
  else if (!missing(prec))
    stopifnot(is.numeric(prec) && length(prec) == 1 && prec > 1)

  if (inherits(n, "rebase") && missing(base) && missing(prec)) {
    exps <- seq(attr(n, "degree"), by = -1, len = NROW(n))
    c(crossprod(attr(n, "base") ^ exps, n))
  } else if (inherits(n, "rebase")) {
    prec <- if (missing(prec)) NROW(n) else floor(prec)
    n <- rebase(n)
    Tailcall(rebase, n, base, prec)
  } else {
    degree <- max(floor(log(n) / log(base) + .Machine$double.eps))
    if (!is.finite(degree)) degree <- 0
    prec <- if (missing(prec)) abs(degree) + 1 else floor(prec)
    if (prec > 30) {
      warning("Precission overflow/underflow")
      prec <- 30
    }
    x <- matrix(double(prec * length(n)), prec, length(n))
    for (i in 1:prec) {
      x[i, ] <- floor(n / base ^ (degree - i + 1))
      n <- n - x[i, ] * base ^ (degree - i + 1)
    }
    if (length(n) == 1L) x <- c(x)
    structure(x, class = "rebase", base = base, degree = degree)
  }
}

#' @rdname rebase
#' @param x a \emph{rebase} object
#' @param ... further arguments passed to base \code{print}
#' @export
#'
print.rebase <- function(x, ...) {
  foot <- with(attributes(x), paste0("degree: ", degree, " base: ", base, "\n"))
  attr(x, "degree") <- NULL
  attr(x, "base") <- NULL
  print(unclass(x), ...)
  cat(foot)
  invisible(x)
}


#' Plot the beautiful Mandelbrot set
#' @description
#' Only an exapmle of the use of Tailcall to plot Mandelbrot point set.
#' @param xlim plot x limits
#' @param ylim plot y limits
#' @param res resolution
#' @param max_depth max depth of iterations to find if a point is in the set
#' @param nclr number of colors
#'
#' @return invisible()
#' @export
#'
#' @examples
#' plot_mandelbrot(x = c(-2, 0.5), y = c(-1, 1), res= 0.02, nclr = 20)
#' plot_mandelbrot(x = c(-0.06, -0.05), y = c(-0.83, -0.82),
#'                res= 0.0005, max_depth = 2000)
plot_mandelbrot <- function(xlim, ylim, res = 0.002,
                            max_depth = 200, nclr = 20) {
  mb_point <- function(c, z = 0i, n = 1L, d = NA) {
    z <- z ^ 2 + c
    d <- ifelse(is.na(d) & Mod(z) > 2, n, d)
    if (n >= max_depth) d else Tailcall(mb_point, c, z, n + 1L, d)
  }
  colors <- function(p) {
    grDevices::hcl.colors(nclr)[nclr * ((p - 1) / max_depth) ^ 0.33 + 1]
  }
  points <- outer(
    seq(xlim[1], xlim[2], res), seq(ylim[1], ylim[2], res),
    \(i, j) mb_point(complex(real = i, imaginary = j)))
  raster <- grDevices::as.raster(
    t(array(colors(points), dim(points))[
      , rev(seq_len(ncol(points)))]))
  plot(xlim, ylim, type = "n")
  graphics::rasterImage(raster, xlim[1], ylim[1], xlim[2], ylim[2])
}
