library(plotly)

#' Find local partial derivatives using the Evans-Young Fit. See Florinsky
#' (2012) 4.1 for details.
#'
#' @param z Vector of exactly 9 numeric elevation values. These correspond
#' to a 3x3 neighborhood like so:
#' 1 2 3
#' 4 5 6
#' 7 8 9
#' @param w Spacing between the elevation values. Default is 1. Must be in the
#' same units as the elevation values.
#'
#' @return A list of the first and second order partial derivatives for the 
#' surface fitted at the center point (= elevation at index 5 in the input 
#' vector).
#' @export
#'
#' @examples
get_evans_young_fit <- function(z, w=1) {
  r <- (z[1]+z[3]+z[4]+z[6]+z[7]+z[9] - 2*(z[2]+z[5]+z[8])) / (3*w^2)
  t <- (z[1]+z[2]+z[3]+z[7]+z[8]+z[9] - 2*(z[4]+z[5]+z[6])) / (3*w^2)
  s <- (z[3]+z[7]-z[1]-z[9]) / (4*w^2)
  p <- (z[3]+z[6]+z[9]-z[1]-z[4]-z[7]) / (6*w)
  q <- (z[1]+z[2]+z[3]-z[7]-z[8]-z[9]) / (6*w)
  u <- 1/9 * (2*(z[2]+z[4]+z[6]+z[8]) - (z[1]+z[3]+z[7]+z[9]) + 5*z[5])
  
  list(
    r=r, t=t, s=s, p=p, q=q, u=u
  )
}

#' Calculate terrain metrics from a vector of elevation values.
#' 
#' Note: flat surfaces will return NaN for curvature.
#'
#' @param z Vector of exactly 9 elevation values, arranged as in
#' get_evans_young_fit.
#' @param w Spacing between the elevation values. Default is 1. Must be in the
#' same units as the elevation values.
#'
#' @return A list with slope, h_curv, and v_curv.
#' @export
#'
#' @examples
get_terrain_metrics <- function(z, w=1) {
  partials <- get_evans_young_fit(z, w)
  r <- partials$r
  t <- partials$t
  s <- partials$s
  p <- partials$p
  q <- partials$q
  
  slope <- atan(sqrt(p^2+q^2)) # in rads
  
  # If p and q are both zero then the curvature result is 0/0, 
  # force to 0 since the surface is flat in this case.
  if (p == 0 & q == 0) {
    h_curv <- 0
    v_curv <- 0
  } else {
    h_curv <- -1 * (q^2*r - 2*p*q*s + p^2*t) /
      ( (p^2 + q^2) * sqrt(1+p^2+q^2) )
    v_curv <- -1 * (p^2*r + 2*p*q*s + q^2*t) /
      ( (p^2 + q^2) * (1+p^2+q^2)^1.5 )
  }
  
  relief <- max(abs(z[5] - z))
  grad   <- relief / w
  
  
  list(slope=slope, h_curv=h_curv, v_curv=v_curv,
       relief=relief, grad=grad)
}

render_evans_young_fit <- function(z, w=1) {
  partials <- get_evans_young_fit(z, w)
  r <- partials$r
  t <- partials$t
  s <- partials$s
  p <- partials$p
  q <- partials$q
  u <- partials$u
  
  # The valid range of the polynomial is *always* [-1, 1] for both x
  # and y axes.
  x_surf <- seq(-1, 1, length.out=100)
  y_surf <- seq(-1, 1, length.out=100)
  z_surf <- matrix(nrow=length(x_surf), ncol=length(y_surf))
  
  # This can be made much more efficient
  for (i in 1:length(x_surf)) {
    for (j in 1:length(y_surf)) {
      # Note flip of j and i here since y is our "row" on the surface.
      z_surf[j, i] <- r*x_surf[i]^2/2 + t*y_surf[j]^2/2 + s*x_surf[i]*y_surf[j] +
        p*x_surf[i] + q*y_surf[j] + u
    }
  }
  
  x_pts <- rep(c(-1, 0, 1), 3)
  y_pts <- c(rep(1, 3), rep(0, 3), rep(-1, 3))
  
  plot_ly() %>%
    add_surface(
      x=x_surf,
      y=y_surf,
      z=z_surf,
      opacity=0.5
    ) %>%
    add_markers(
      x=x_pts,
      y=y_pts,
      z=z
    )
}

# Test surface
z1 <- c(1, 4, 4,
        3, 3, 4,
        3, 4, 5)

p <- render_evans_young_fit(z1)
