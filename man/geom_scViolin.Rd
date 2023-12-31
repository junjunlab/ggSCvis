% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_scViolin.R
\name{geom_scViolin}
\alias{geom_scViolin}
\title{Custom ggplot2 geometry: violin plot}
\usage{
geom_scViolin(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  show.legend = NA,
  na.rm = FALSE,
  inherit.aes = TRUE,
  ...,
  add.tree = FALSE,
  tree.type = "rectangle",
  add.tree.y = FALSE,
  tree.y.side = "left",
  tree.y.width = 0.06,
  add.tree.x = FALSE,
  tree.x.side = "bottom",
  tree.x.height = 0.06,
  tree.y.label.hjust = 1,
  tree.x.label.hjust = 1,
  new.ylabel.x = -0.08,
  new.ylabel.width = 0.025,
  new.ylabel.size = 8,
  new.ylabel.face = "plain",
  new.ylabel.rot = 0,
  new.xlabel.y = -0.08,
  new.xlabel.height = 0.025,
  new.xlabel.size = 8,
  new.xlabel.face = "plain",
  new.xlabel.rot = 90,
  tree.x.shift = 0,
  tree.y.shift = 0,
  branch.side = "top",
  branch.shift = 0,
  branch.height = 0.05,
  branch.len = 0.8,
  branch.lwd = 0.5,
  branch.label.rot = NULL,
  branch.label.hjust = NULL,
  branch.label.size = 10,
  branch.label.shift = 0.2,
  trim = TRUE
)
}
\arguments{
\item{mapping}{Aesthetic mapping.}

\item{data}{A data frame.}

\item{stat}{The statistical transformation to use (default is "identity").}

\item{position}{The position adjustment to use (default is "identity").}

\item{show.legend}{Logical value indicating whether to show a legend for the layer (default is NA).}

\item{na.rm}{Logical value indicating whether to remove missing values (default is FALSE).}

\item{inherit.aes}{Logical value indicating whether to inherit aesthetics from the plot (default is TRUE).}

\item{...}{Additional arguments to be passed to \code{\link[ggplot2]{geom_point}}.}

\item{add.tree}{Whether add tree around the plot (default is FALSE).}

\item{tree.type}{Type of dendrogram tree ("rectangle" or "triangle").}

\item{add.tree.y}{Whether to add a vertical dendrogram.}

\item{tree.y.side}{Side for the vertical dendrogram ("left" or "right").}

\item{tree.y.width}{Width of the vertical dendrogram.}

\item{add.tree.x}{Whether to add a horizontal dendrogram.}

\item{tree.x.side}{Side for the horizontal dendrogram ("bottom" or "top").}

\item{tree.x.height}{Height of the horizontal dendrogram.}

\item{tree.y.label.hjust}{Horizontal justification for y-axis labels (0 or 1).}

\item{tree.x.label.hjust}{Horizontal justification for x-axis labels (0 or 1).}

\item{new.ylabel.x}{X-coordinate for the new y-axis labels.}

\item{new.ylabel.width}{Width of the new y-axis labels.}

\item{new.ylabel.size}{Font size for the new y-axis labels.}

\item{new.ylabel.face}{Font face for the new y-axis labels.}

\item{new.ylabel.rot}{Rotation angle for the new y-axis labels.}

\item{new.xlabel.y}{Y-coordinate for the new x-axis labels.}

\item{new.xlabel.height}{Height of the new x-axis labels.}

\item{new.xlabel.size}{Font size for the new x-axis labels.}

\item{new.xlabel.face}{Font face for the new x-axis labels.}

\item{new.xlabel.rot}{Rotation angle for the new x-axis labels.}

\item{tree.x.shift}{Shift for the dendrogram placement.}

\item{tree.y.shift}{Shift for the dendrogram placement.}

\item{branch.side}{The side of the annotation segment (one of "top", "right", "bottom", or "left").}

\item{branch.shift}{The shift of the annotation segment along its side.}

\item{branch.height}{The height of the annotation segment.}

\item{branch.len}{The length of the annotation segment.}

\item{branch.lwd}{The line width of the annotation segment.}

\item{branch.label.rot}{The angle of labels on the annotation segment.}

\item{branch.label.hjust}{The hjust of labels on the annotation segment.}

\item{branch.label.size}{The font size of labels on the annotation segment.}

\item{branch.label.shift}{The shift of labels from the annotation segment.}

\item{trim}{If TRUE (default), trim the tails of the violins to the range of the data.
If FALSE, don't trim the tails.}
}
\value{
A ggplot2 layer.
}
\description{
This custom ggplot2 geometry function, \code{geom_scDot}, allows you to create
violin plots in ggplot2.
}
