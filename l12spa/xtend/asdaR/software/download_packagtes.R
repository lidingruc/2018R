#R Script to download packages

l<-read.table(file="pkgs.txt", header=FALSE)

#Download R

download.file("http://cran.r-project.org/src/base/R-2/R-2.5.1.tar.gz", "src/R-2.5.1.tar.gz")
download.file("http://cran.r-project.org/bin/macosx/R-2.5.1.dmg", "mac-binary/R-2.5.1.dmg")
download.file("http://cran.r-project.org/bin/windows/base/R-2.5.1-win32.exe", "win-binary/R-2.5.1-win32.exe")

#Download packages

download.packages( levels(l$V1), type="source", destdir="src")
download.packages( levels(l$V1), type="mac.binary", destdir="mac-binary")
download.packages( levels(l$V1), type="win.binary", destdir="win-binary")


