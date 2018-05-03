\name{renameClass}
\alias{renameClass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Konsistentes Umbenennen von Klassen
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
renameClass(oldCls,oldName,newName) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oldCls}{
Spaltenvektor der Klasssennamen (nummern)
}
  \item{oldName}{
 diese Klassennummer ist zu aendern 
}  
\item{newName}{
  in diesen 
}

}

\value{
 newCls    alle Vorkomnisse von oldName sind gegen newName ausgetauscht 
 
          und alle Vorkomnisse von newName sind gegen oldName ausgetauscht
}

\author{
MT
}