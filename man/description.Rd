\name{directotree}
\alias{directotree}
\title{Creates an Interactive Tree Structure of a Directory}

\description{Represents the content of a directory as an interactive collapsible tree. Offers
    the possibility to assign a text (e.g., a 'Readme.txt') to each folder
    (represented as a clickable node), so that when the user hovers the pointer
    over a node, the corresponding text is displayed as a tooltip.}

\usage{directotree(yourPath, showWd = FALSE, warnMe = FALSE, addReadme = FALSE,
            attributeName = '', hiddenFiles = c('Thumbs.db'), showRawTree = FALSE,
            showCollapsibleTree = TRUE, tooltip = FALSE, linkLength = 150,
            collapsed = TRUE, zoomable = TRUE, width = 1240, height = 700,
            fontSize = 15, fill = 'lightblue', nodeSize = 'leafCount')}

\arguments{
  \item{yourPath}{
  The path of the directory you want to build a interactive representation of. It must be a string, and make sure to use / instead of \\.
}
  \item{showWd}{
  If TRUE, R prints each directory it goes in to attach files/folders to the parent node.
  }
  \item{warnMe}{
  If TRUE, R prints a Warning when an empty folder is detected, and indicates its location.
  }
  \item{addReadme}{
  If TRUE, the content of the files whose name must be specified in attributeName, will be attached to each corresponding folder. For instance, if in some subfolders of yourPath, some Readme.txt files describe the content of the said folders, by setting addReadme to TRUE and attributeName to 'Readme.txt', the description written in the Readme.txt files will be attached to the nodes of the corresponding folders as a tooltip (tooltip must be set to TRUE).
  }
  \item{attributeName}{
  If addReadme is set to TRUE, attributeName must be filled with the name of the files whose content should be displayed as tooltips (e.i., 'Readme.txt', 'README.txt', 'info.txt', etc.).
  }
  \item{hiddenFiles}{
  Vector of strings that must contain the names of the files that must not be displayed in the tree. For instance, you may want to display the content of some 'Readme.txt' files as tooltips, without them being showed as leaves of the tree.
  }
  \item{showRawTree}{
  It TRUE, the tree structure is printed in the console.
  }
  \item{showCollapsibleTree}{
  If TRUE, calls the collapsibleTree function to build the interactive tree.
  }
  \item{tooltip}{
  If TRUE, it shows the node’s label and attribute value.
  }
  \item{linkLength}{
  Length of the horizontal links that connect nodes in pixels.
  }
  \item{collapsed}{
  If TRUE, the tree’s children will start collapsed by default.
  }
  \item{zoomable}{
  Pan and zoom by dragging and scrolling.
  }
  \item{width}{
  Width in pixels.
  }
  \item{height}{
  Height in pixels.
  }
  \item{fontSize}{
  Font size of the label text in pixels.
  }
  \item{fill}{
  A tree attribute containing the color for each node.
  }
  \item{nodeSize}{
  Numeric column that will be used to determine relative node size. ’leafCount’
(cumulative count of a node’s children), or ’count’ (count of node’s immediate
children) can also be used.
  }
}

\author{Louis Sirugue}

\seealso{
<https://cran.r-project.org/web/packages/data.tree/data.tree.pdf>

<https://cran.r-project.org/web/packages/collapsibleTree/collapsibleTree.pdf>
}
\examples{
directotree(getwd(), addReadme = FALSE, tooltip = FALSE, showRawTree = FALSE,
showCollapsibleTree = TRUE, hiddenFiles = c('Readme.txt', 'Thumbs.db'))


}

\keyword{ ~tree }
\keyword{ ~dynamic }
