library(grid)

# Smooth [polynomial] version of terrain fill.
# x and y determine canvas size.
fillsmooth <- function(x=25, y=25) fill(polyinterp, x, y)

# Straight [linear] version of terrain fill.
# x and y determine canvas size.
fillstraight <- function(x=25, y=25) fill(lininterp, x, y)

# Plot a grid and allow the user to place columns on it.
# Fill between the columns in red using a selected interpolation function.
# Useful for Minecraft design.
fill <- function(mode, x=25, y=25) {
	# Use grid library features to create a borderless grid.
	par(mar=rep(0, 4))
	plot(
		NULL, xlim=c(0, x), ylim=c(0, y),
		axes=FALSE, frame.plot=FALSE,
		xlab=NA, ylab=NA, xaxs="i", yaxs="i"
	)
	grid(x, y)
	par(mar=rep(0,4))

	# Macro: plot a rectangle of specified height and color
	# at a specified horizontal position rectx.
	# Destroy any previous rectangle at that horizontal position.
	makerect <- function(rectx, recty=0, color="black") {
		rectname <- as.character(rectx)
		while (rectname %in% getNames()) {
			grid.remove(rectname)
		}
		grid.rect(
			x=unit(rectx/x, "npc"),
			y=unit(0, "npc"),
			width=unit(1/x, "npc"),
			height=unit(recty/y, "npc"),
			just=c("left", "bottom"),
			gp=gpar(fill=color),
			name=rectname
		)
	}

	# Initialize height-0 rectangles at all positions.
	sapply(0:25, makerect)

	# Internal matrix representation of user-created rectangle
	# positions and heights.
	points <- matrix(c(1:x, rep(NA, x)), x, 2)

	# GUI loop. Create black rectangles on user click.
	# Destroy and redraw the rectangle if the user clicks on the
	# same column of the grid again.
	# This loop exits once the user enters a right-click
	# instead of a left-click and selects "quit",
	# causing grid.locator() to return null.
	while (TRUE) {
		pos <- grid.locator("npc")
		if (is.null(pos)) break

		rectx <- floor(as.numeric(pos$x) * x)
		recty <- ceiling(as.numeric(pos$y) * y)
		makerect(rectx, recty)
		points[rectx + 1, 2] <- recty
		grid(x, y)
	}

	# Finalize the representation of user-submitted data
	# and send it to the interpolator.
	points <- points[!is.na(points[, 2]), , drop=FALSE]
	x_points <- points[, 1]
	interpolator <- mode(points)

	# Use the interpolator to plot red rectangles with interpolated heights
	# but only between user-created black rectangles.
	for (i in 1:x) {
		if (i <= min(x_points) | i >= max(x_points) | i %in% x_points) {
			next
		}
		makerect(i - 1, interpolator(i), "red")
	}
	grid(x, y)
}

# Given a matrix with x- and y-coordinate columns
# representing a list of points, return a function
# that interpolates those points with a polynomial
# of minimal degree by inverting a Vandermonde matrix.
# The output is rounded to the nearest integer.
polyinterp <- function(points) {
	xs <- points[, 1]
	points <- points[!(xs %in% (xs + 1) & xs %in% (xs - 1)), ]
	xs <- points[, 1]
	ys <- points[, 2]
	V <- outer(xs, seq(0, length(xs) - 1), `^`)
	A <- solve(V) %*% ys
	n <- length(A) - 1

	function(x) {
		as.integer(round(sum(x^(0:n) * A)))
	}	
}

# Given a matrix with x- and y-coordinate columns
# representing a list of points, return a function
# that interpolates those points piecewise-linearly.
# This function's argument x must be an integer between
# the horizontal extrema of the original points,
# and the output is rounded to the nearest integer.
lininterp <- function(points) {
	xs <- points[, 1]
	x_min <- min(xs)
	x_max <- max(xs)
	x_range <- x_min:x_max

	A <- merge(
		data.frame(x_range),
		data.frame(points),
		by.x = 1,
		by.y = 1,
		all.x = TRUE
	)
	colnames(A) <- c("x", "y")

	for (i in 1:(dim(A)[1])) {
		x <- A[i, ]$x
		y <- A[i, ]$y
		if(is.na(y)) {
			x_right <- min(which(!is.na(A$y) & A$x > x))
			left <- A[x_left, ]
			right <- A[x_right, ]
			m <- (right$y - left$y) / (right$x - left$x)
			b <- left$y - m * left$x
			A[i, ]$y <- as.integer(round(m * x + b))
		} else {
			x_left <- i
		}
	}		

	function(x) {
		A[which(A$x == x)[1], 2]
	}
}