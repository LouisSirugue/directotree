directotree <- function(yourPath, showWd = FALSE, warnMe = FALSE, addReadme = FALSE,
                        attributeName = "", hiddenFiles = c("Thumbs.db"), showRawTree = FALSE,
                        showCollapsibleTree = TRUE, tooltip = FALSE, linkLength = 150,
                        collapsed = TRUE, zoomable = TRUE, width = 1240, height = 700,
                        fontSize = 15, fill = 'lightblue', nodeSize = 'leafCount'){

  requireNamespace(c("data.tree", "collapsibleTree"), quietly = TRUE)

  setwd(yourPath)
  stratum <- 0
  decomp <- strsplit(yourPath, "/")
  decomp2 <- unlist(decomp, use.names=FALSE)
  last <- length(decomp2)
  parentlabel <- decomp2[last]

  parentnode <- Node$new(parentlabel)
  parentfold0 <-  parentnode

  line <- list.files(recursive = FALSE)
  if (addReadme == TRUE){
    attributeToF <- tryCatch(match(attributeName, line), error = c)
    if (is.na(attributeToF ) == FALSE) {
      readme <- paste(readLines(con = attributeName, n = -1L, ok = TRUE, warn = FALSE,
                                encoding = "unknown", skipNul = FALSE))
      nblines <- length(readme)
      attribute <- character(0)
      for (i in readme){
        currentline <- match(i, readme)
        attribute <- paste(attribute, i, sep = " ")
        if (currentline < nblines){
          if (i == "") {
            attribute <- paste(attribute, "<br><br>", sep = "")
          }
        }
      }
      parentfold0$Note <- attribute
    }
  }

  folders <- character(0)
  files <- character(0)
  for (i in line) {
    erreur <- tryCatch(setwd(i), error = c)
    lerreur <- length(erreur)
    position <- match(i, line)
    if (lerreur == 1) {
      folders <- c(folders, line[position])
      setwd("..")
    } else {
      files <- c(files, line[position])
    }
  }

  nbfolders <- length(folders)
  nbfiles <- length(files)
  positionk <- 0
  Imhere <- getwd()
  if (showWd == TRUE) {
    print(Imhere)
  }

  nbelements <- length(line)
  if (nbelements == 0) {
    Warning <- paste("This directory contains no element:", Imhere, sep = " ")
    if (warnMe == TRUE) {
      warning(Warning)
    }
  }

  if (Imhere != yourPath || positionk != nbfolders) {
    while (Imhere != yourPath || positionk != nbfolders) {

      if (positionk != nbfolders) {
        positionl <- positionk + 1
        nextfold <- folders[positionl]
        setwd(nextfold)
        nextfold <- get(paste("parentfold", stratum, sep = ""))$AddChild(nextfold)
        stratum <- stratum + 1
        Imhere <- getwd()
        if (showWd == TRUE) {
          print(Imhere)
        }
        parentfold <- nextfold
        assign(paste("parentfold", stratum, sep = ""), nextfold)

        line <- list.files(recursive = FALSE)
        attributeToF <- tryCatch(match(attributeName, line), error = c) ##ICI
        if (is.na(attributeToF ) == FALSE) {
          readme <- paste(readLines(con = attributeName, n = -1L, ok = TRUE, warn = FALSE,
                                    encoding = "unknown", skipNul = FALSE))
          nblines <- length(readme)
          attribute <- character(0)
          for (i in readme){
            currentline <- match(i, readme)
            attribute <- paste(attribute, i, sep = " ")
            if (currentline < nblines){
              if (i == "") {
                attribute <- paste(attribute, "<br><br>", sep = "")
              }
            }
          }
          nextfold$Note <- attribute
        }


        folders <- character(0)
        files <- character(0)
        for (i in line) {
          erreur <- tryCatch(setwd(i), error = c)
          lerreur <- length(erreur)
          position <- match(i, line)
          if (lerreur == 1) {
            folders <- c(folders, line[position])
            setwd("..")
          } else {
            files <- c(files, line[position])
          }
        }

        nbfolders <- length(folders)

        while (nbfolders > 0) {
          first <- folders[1]
          setwd(first)
          stratum <- stratum + 1
          Imhere <- getwd()
          if (showWd == TRUE) {
            print(Imhere)
          }
          first <- parentfold$AddChild(first)
          parentfold <- first
          assign(paste("parentfold", stratum, sep = ""), first)

          line <- list.files(recursive = FALSE)
          attributeToF <- tryCatch(match(attributeName, line), error = c)
          if (is.na(attributeToF ) == FALSE) {
            readme <- paste(readLines(con = attributeName, n = -1L, ok = TRUE, warn = FALSE,
                                      encoding = "unknown", skipNul = FALSE))
            nblines <- length(readme)
            attribute <- character(0)
            for (i in readme){
              currentline <- match(i, readme)
              attribute <- paste(attribute, i, sep = " ")
              if (currentline < nblines){
                if (i == "") {
                  attribute <- paste(attribute, "<br><br>", sep = "")
                }
              }
            }
            first$Note <- attribute
          }

          folders <- character(0)
          files <- character(0)
          for (i in line) {
            erreur <- tryCatch(setwd(i), error = c)
            lerreur <- length(erreur)
            position <- match(i, line)
            if (lerreur == 1) {
              folders <- c(folders, line[position])
              setwd("..")
            } else {
              files <- c(files, line[position])
            }
          }

          nbfolders <- length(folders)
        }

        nbfiles <- length(files)
        if (nbfiles > 0) {
          for (i in files){
            if (is.na(match(i, hiddenFiles)) == TRUE) {
              i <- parentfold$AddChild(i)
            }
          }
        } else {
          ici <- getwd()
          Warning <- paste("Empty folder detected:", ici, sep = " ")
          if (warnMe == TRUE) {
            warning(Warning)
          }
        }

        prevpath <- getwd()
        setwd("..")
        stratum <- stratum - 1
        Imhere <- getwd()
        if (showWd == TRUE) {
          print(Imhere)
        }

        line <- list.files(recursive = FALSE)
        folders <- character(0)
        files <- character(0)
        for (i in line) {
          erreur <- tryCatch(setwd(i), error = c)
          lerreur <- length(erreur)
          position <- match(i, line)
          if (lerreur == 1) {
            folders <- c(folders, line[position])
            setwd("..")
          } else {
            files <- c(files, line[position])
          }
        }

        nbfolders <- length(folders)
        nbfiles <- length(files)

        if (nbfolders < 2) {
          while (nbfolders < 2 && Imhere != yourPath) {
            if (nbfiles > 0) {
              for (i in files){
                if (is.na(match(i, hiddenFiles)) == TRUE) {
                  i <- get(paste("parentfold", stratum, sep = ""))$AddChild(i)
                }

              }
            }

            prevpath <- getwd()
            setwd("..")
            stratum <- stratum - 1
            Imhere <- getwd()
            if (showWd == TRUE) {
              print(Imhere)
            }

            line <- list.files(recursive = FALSE)
            folders <- character(0)
            files <- character(0)
            for (i in line) {
              erreur <- tryCatch(setwd(i), error = c)
              lerreur <- length(erreur)
              position <- match(i, line)
              if (lerreur == 1) {
                folders <- c(folders, line[position])
                setwd("..")
              } else {
                files <- c(files, line[position])
              }
            }

            nbfolders <- length(folders)
            nbfiles <- length(files)
          }
        }
        line <- list.files(recursive = FALSE)
        folders <- character(0)
        files <- character(0)
        for (i in line) {
          erreur <- tryCatch(setwd(i), error = c)
          lerreur <- length(erreur)
          position <- match(i, line)
          if (lerreur == 1) {
            folders <- c(folders, line[position])
            setwd("..")
          } else {
            files <- c(files, line[position])
          }
        }
        nbfolders <- length(folders)
        nbfiles <- length(files)

        decomp2 <- strsplit(prevpath, "/")
        decomp22 <- unlist(decomp2, use.names=FALSE)
        last2 <- length(decomp22)
        whereIcomefrom <- decomp22[last2]

        for (k in folders) {
          if (k == whereIcomefrom){
            break
          }
        }
        positionk <- match(k,folders)
        Imhere <- getwd()
      }

      if (Imhere != yourPath && positionk == nbfolders){
        if (nbfiles != 0) {
          for (i in files){
            if (is.na(match(i, hiddenFiles)) == TRUE) {
              i <- get(paste("parentfold", stratum, sep = ""))$AddChild(i)
            }
          }
        }

        prevpath <- getwd()
        setwd("..")
        stratum <- stratum - 1
        Imhere <- getwd()
        if (showWd == TRUE) {
          print(Imhere)
        }
        line <- list.files(recursive = FALSE)
        folders <- character(0)
        files <- character(0)
        for (i in line) {
          erreur <- tryCatch(setwd(i), error = c)
          lerreur <- length(erreur)
          position <- match(i, line)
          if (lerreur == 1) {
            folders <- c(folders, line[position])
            setwd("..")
          } else {
            files <- c(files, line[position])
          }
        }
        nbfolders <- length(folders)
        nbfiles <- length(files)

        decomp2 <- strsplit(prevpath, "/")
        decomp22 <- unlist(decomp2, use.names=FALSE)
        last2 <- length(decomp22)
        whereIcomefrom <- decomp22[last2]

        for (k in folders) {
          if (k == whereIcomefrom){
            break
          }
        }
        positionk <- match(k,folders)
        Imhere <- getwd()
      }
    }
  }

  line <- list.files(recursive = FALSE)
  folders <- character(0)
  files <- character(0)
  for (i in line) {
    erreur <- tryCatch(setwd(i), error = c)
    lerreur <- length(erreur)
    position <- match(i, line)
    if (lerreur == 1) {
      folders <- c(folders, line[position])
      setwd("..")
    } else {
      files <- c(files, line[position])
    }
  }
  nbfiles <- length(files)
  if (nbfiles != 0) {
    for (i in files){
      if (is.na(match(i, hiddenFiles)) == TRUE) {
        i <- parentnode$AddChild(i)
      }
    }
  }

  if (showRawTree == TRUE) {
    if (addReadme == TRUE) {
      return(print(parentnode, "Note"))
    } else {
      return(parentnode)
    }
  }

  if (showCollapsibleTree == TRUE) {
    if (addReadme == TRUE) {
      collapsibleTree(parentnode, attribute = "Note", tooltip = tooltip, linkLength = linkLength,
                      collapsed = collapsed, zoomable = zoomable, width = width, height = height,
                      fontSize = fontSize, fill = fill, nodeSize = nodeSize)
    } else {
      collapsibleTree(parentnode, tooltip = tooltip, linkLength = linkLength,
                      collapsed = collapsed, zoomable = zoomable, width = width, height = height,
                      fontSize = fontSize, fill = fill, nodeSize = nodeSize)
    }
  }
}
