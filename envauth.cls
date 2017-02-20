%---------------------------------------------------------------------------
%Please  be  aware that the use of this LaTeX class file is governed by the
%following conditions:
%
% based on the original LaTeX ARTICLE DOCUMENT STYLE
% Copyright (C) 1988, 1989 by Leslie Lamport
%
% Copyright (c) 2008 John Wiley & Sons, Ltd, The Atrium, Southern Gate, Chichester,
% West Sussex, PO19 8SQ UK.   All rights reserved.
%
%Rules of Use
%
%% You are NOT ALLOWED to change this file.
%
%
%This class file is made available for use by authors who wish to prepare an
%article for publication in
%ENVIRONMETRICS
%published by John Wiley & Sons Ltd. The user may not exploit any part of
%the class file commercially.
%
%This class file is provided on an `as is'  basis, without warranties of any
%kind, either expressed or implied, including but not limited to warranties of
%title, or  implied warranties of merchantablility or fitness for  a
%particular purpose. There will be no duty on the author[s] of the software
%or John Wiley & Sons Ltd to correct any errors or defects in the software.
%Any statutory rights you may have remain unaffected by your acceptance of
%these rules of use.
% -----------------------------------------------------------------------

\def\update{2012/07/12 v1.00}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{envauth}[\update\ Environmetrics]

%\newcommand\hmmax{0}

\newif\if@cmbrightfont
\DeclareOption{cmbright}{%
\@cmbrightfonttrue}

\newif\if@doublespace
\DeclareOption{doublespace}{%
\@doublespacetrue}

%%%%%%%%%%%%%%%%

\newif\ifreferee

\DeclareOption{referee}{\refereetrue\@doublespacetrue\@cmbrightfontfalse}

%%%%%%%%%%%%%%%%%%%

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\if@cmbrightfont
\RequirePackage{cmbright}
\fi
\if@doublespace
\RequirePackage[doublespacing]{setspace}
\fi

\RequirePackage{graphicx}
\RequirePackage{pifont,latexsym,ifthen,theorem,rotating,calc,textcase,booktabs,color}
\RequirePackage{amsfonts,amssymb,amsbsy,amsmath}
%\RequirePackage{bm}
\RequirePackage[errorshow]{tracefnt}
\usepackage{helvet}


\@twosidetrue
\flushbottom
\frenchspacing
\ifreferee
\textwidth 165mm
\textheight 220mm
\topskip 26pt
\footskip 24pt 
\topmargin 5mm
\setlength\oddsidemargin{1in}%back margin on odd pages
\setlength\evensidemargin{1in}%fore margin on even pages
\else
\textwidth 176mm%41.6pc
\textheight 240mm%682pt
\footskip 18pt 
\topmargin 0mm
\setlength\oddsidemargin{18mm}%back margin on odd pages
\setlength\evensidemargin{21.5mm}%fore margin on even pages
\fi

%\setlength\columnsep{24pt}

%Trim sizes
\setlength\voffset{-1.2in}
\setlength\hoffset{-1in}
\setlength\paperwidth{210mm}
\setlength\paperheight{279mm}
%Needed to set PDF page size
\special{papersize=210mm,279mm}

\parskip \z@
\parindent 9pt
\headheight 50pt
%\headsep    30pt
\headsep    24pt


\brokenpenalty=10000%
\clubpenalty=9999%
\widowpenalty=9999%
\hyphenpenalty=9999%
\finalhyphendemerits=14000%
\doublehyphendemerits=75000%
\lefthyphenmin=3%
\righthyphenmin=3%
\pretolerance=1000%
\tolerance=2000%
\hfuzz=0.99pt%
\hbadness=8000
\vbadness=9000

\thinmuskip = 3mu
\medmuskip = 4mu
\thickmuskip = 5mu

\setcounter{topnumber}{10}
\def\topfraction{1}
\setcounter{bottomnumber}{10}
\def\bottomfraction{0.8}
\setcounter{totalnumber}{10}
\def\textfraction{0}
\renewcommand{\floatpagefraction}{0.95}
\setcounter{dbltopnumber}{10}
\renewcommand{\dblfloatpagefraction}{0.95}
\renewcommand{\dbltopfraction}{1}

\ifreferee
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xiipt{14pt plus 0.10pt minus 0.10pt}%
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 6\p@ \@plus2\p@ \@minus2\p@
               \parsep 3\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\else
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@ixpt{11pt plus 0.10pt minus 0.10pt}%
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 6\p@ \@plus2\p@ \@minus2\p@
               \parsep 3\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\fi


\normalsize

\renewcommand\small{%
   \@setfontsize\small\@viiipt{10}%
   \abovedisplayskip 8\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\renewcommand{\footnotesize}{\fontsize{7.5pt}{9.5pt}\selectfont}
\renewcommand\scriptsize{\@setfontsize\scriptsize\@viipt{9.5}}
\renewcommand\tiny{\@setfontsize\tiny\@vpt\@viipt}
\renewcommand\large{\@setfontsize\large\@xipt{13}}
\renewcommand\Large{\@setfontsize\Large\@xivpt{18}}
\renewcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\renewcommand\huge{\fontsize{22pt}{24pt}\selectfont}
\renewcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\ifreferee
\newcommand{\sectionsize}{\fontsize{12pt}{12pt}\selectfont}
\newcommand{\titlesize}{\fontsize{22pt}{14pt}\selectfont}
\else
\newcommand{\sectionsize}{\fontsize{12pt}{14pt}\selectfont}
\newcommand{\titlesize}{\fontsize{22pt}{24pt}\selectfont}
\fi

\newcommand{\authorsize}{\fontsize{15pt}{17pt}\selectfont}

\def\journalname{ENVIRONMETRICS}
\def\journalabb{Environmetrics}

\newbox\absbox
\ifreferee
\def\abstract{\lrbox\absbox\minipage{\textwidth}%
  \fontsize{10pt}{12pt}\selectfont{\bfseries Summary:}%
  }
\else
\def\abstract{\lrbox\absbox\hskip13mm\minipage{163mm}%
  \sf\normalsize\bfseries%
  }
\fi
  
  
\def\endabstract{%\copyrightline
\endminipage\endlrbox}

\ifreferee
\def\keywords#1{%
  \gdef\@keywords{\fontsize{10}{12}\selectfont{\textbf{Keywords:}}\hspace{0.75em}\parbox[t]{\textwidth}{#1}}}
\else
\def\keywords#1{%
  \gdef\@keywords{{\sffamily%
    \hskip13mm\textbf{Keywords:}}\hspace{0.75em}\parbox[t]{163mm}{\sf#1}}}
\fi
    
    
\let\@keywords\@empty

\def\received#1{%
\gdef\@received{Received #1}}
\let\@received\@empty
\def\revised#1{%
\gdef\@revised{Revised #1}}
\let\@revised\@empty
\def\accepted#1{%
\gdef\@accepted{Accepted #1}}
\let\@accepted\@empty

\skip\footins 22pt plus  8pt
\gdef\footnoterule{}

\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\long\def\@makefntext#1{\parindent 1em%
\noindent{$\m@th^{\@thefnmark}$}#1}

\ifreferee
\def\corraddr#1{%
  \gdef\@corraddr{%
    \footnotetext[1]{\itshape Correspondence to: #1}\stepcounter{footnote}}}
\else
\def\corraddr#1{%
  \gdef\@corraddr{%
    \footnotetext[1]{\sf\itshape Correspondence to: #1}\stepcounter{footnote}}}
\fi

    
\let\@corraddr\@empty
\def\corrauth{\footnotemark[1]}

\ifreferee
\def\address#1{%
  \gdef\@address{%
    \footnotetext[0]{\noindent{\rule{60mm}{0.5pt}}\vskip 4pt\noindent\itshape #1}}}
\else
\def\address#1{%
  \gdef\@address{%
    \footnotetext[0]{\noindent{\color{blue}\rule{\textwidth}{5.0pt}}\vskip 4pt\noindent\sf\itshape #1}}}
\fi
    
\let\@address\@empty

\def\cgs#1{%
  \gdef\@cgs{%
    \footnotetext[0]{\par\noindent\itshape Contract/grant sponsor:  #1}}}
\let\@cgs\@empty

\def\affilnum#1{${}^{\text{\bf#1}}$}
\def\affil#1{${}^{\text{\bf#1}}$}
\def\comma{$^{\text{\bf,}}$}

\renewcommand\maketitle{\par
  \begingroup
     \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
       \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{title}\label{FirstPage}\@address\@corraddr\@cgs
  \endgroup
  %\setcounter{footnote}{0}%
  \global\let\address\relax
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@address\@empty
  \global\let\corraddr\relax
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}
\ifreferee
\def\@maketitle{
  \null
 % \vskip 6pt %
    {\titlesize\bfseries\raggedright\@title \par}%
    \vskip 1.5em%
    {\authorsize
      \lineskip .5em%
      %
        \raggedright\bfseries\@author
      \par}%
      \vskip 3pt
     \vskip 18pt%
    {\noindent\usebox\absbox\par}
    {\lineskip 1.5em%
      %
      {\noindent\normalsize\@keywords}\par}
      \vspace{7pt}
      \par
  \vskip 0.5em}

\else

\def\@maketitle{\vspace*{-20pt}%
\noindent{\sffamily\scriptsize{\@received\quad\quad \@revised\quad\quad \@accepted}}\hfill\mbox{}\par
\vspace{10pt}
\noindent{\sffamily\normalsize(wileyonlinelibrary.com) DOI: 10.1002/\DOI}\\
%\noindent{\sffamily\normalsize MOS subject classification: \@MOSsc}%
  \null
  \vskip 12pt %
    {\titlesize\bfseries\raggedright\@title \par}%
    \vskip 1.5em%
    {\authorsize
      \lineskip .5em%
      %
        \raggedright\sffamily\bfseries\@author
      \par}%
      \vskip 3pt
 %{\normalsize
%      \lineskip .5em%
%      %
%        {\raggedright\emph\@address}
%      \par}%     
    \vskip 18pt%
    {\noindent\usebox\absbox\par}
    {\lineskip 1.5em%
      %
      {\noindent\normalsize\@keywords}\par}
      \vspace{7pt}
    \noindent{\color{blue}\rule{\textwidth}{5.0pt}}
    \par
  \vskip 0.5em}
\fi
  
 
\def\startpage{\pageref{FirstPage}}
\def\endpage{\pageref{LastPage}}
\def\volumeyear{2012}
\def\volumenumber{00}
\def\DOI{env.XXXX}

\gdef\copyrightline{Copyright \copyright\ \volumeyear\ John Wiley \& Sons, Ltd.}
\def\runninghead#1#2{\markboth{#1}{#2}}
%\def\runninghead#1{\markboth{{#1}}{\bfseries {Environmetrics}}}

\gdef\MOS#1{\gdef\@MOSsc{#1}}

\ifreferee
\gdef\rhtext{{\normalsize\begin{tabular}[b]{@{}l@{}}\bfseries\Large Environmetrics\end{tabular}}}
\else
\gdef\rhtext{{\normalsize\sffamily\begin{tabular}[b]{@{}l@{}}\bfseries\Large Environmetrics\end{tabular}}}
\fi


\gdef\ssboxnum{\colorbox{black}{\sffamily\normalsize\textcolor{white}{\thepage}}}

\ifreferee
\def\articletype{}
\else
\def\articletype{Research Article}
\fi

\ifreferee
\def\ps@title{%
\def\@oddhead{\parbox{\textwidth}{\vskip12mm%
{\Large\bfseries Environmetrics \volumenumber, \startpage--\endpage}\hfill{}\\[2pt]
DOI: 10.1002/\DOI\\
%\noindent{\color{blue}\rule{\textwidth}{5.0pt}}%
}}%
\let\@evenhead\@oddhead
\def\@oddfoot{\parbox[t]{\textwidth}{%
%\noindent{\color{blue}\rule{\textwidth}{5.0pt}}
\par\vskip -2pt
{\centerline{This paper has been submitted for consideration for publication in {\it Environmetrics}}
}}}
\let\@evenfoot\@oddfoot
}
\else
\def\ps@title{%
\def\@oddhead{\parbox{\textwidth}{%
{\Large\sffamily\bfseries\color{blue}\articletype}\hfill{\rhtext}\\[-2pt]
\noindent{\color{blue}\rule{\textwidth}{5.0pt}}%
}}%
\let\@evenhead\@oddhead
\def\@oddfoot{\parbox[t]{\textwidth}{%
\noindent{\color{blue}\rule{\textwidth}{5.0pt}}\par\vskip -2pt
{\sffamily\small\emph{Environmetrics}\ {\bfseries\volumeyear}, \volumenumber\ \startpage--\endpage\hfill\copyrightline\hfill\mbox{}\\
\sffamily\textit{Prepared using \textsf{envauth.cls} [Version: \update]}\mbox{}\\
}}}


\let\@evenfoot\@oddfoot}
\fi


\def\ps@env{%
\ifreferee
\def\@evenhead{\parbox{\textwidth}{{\rhtext\hfill\normalsize\leftmark}\\[-2pt]
\noindent{\rule{\textwidth}{1pt}}}}
\def\@oddhead{\parbox{\textwidth}{{\normalsize\rightmark\hfill\rhtext}\\[-2pt]
\noindent{\rule{\textwidth}{1pt}}}}
\else
\def\@evenhead{\parbox{\textwidth}{{\sffamily\rhtext\hfill\normalsize\leftmark}\\[-2pt]
\noindent{\color{blue}\rule{\textwidth}{5.0pt}}}}
\def\@oddhead{\parbox{\textwidth}{{\sffamily\normalsize\rightmark\hfill\rhtext}\\[-2pt]
\noindent{\color{blue}\rule{\textwidth}{5.0pt}}}}
\fi

\ifreferee
\def\@evenfoot{\centerline{\thepage}}
\else
\def\@evenfoot{\parbox[t]{\textwidth}{\noindent{\color{blue}\rule{\textwidth}{5.0pt}}\par
\sffamily\ssboxnum\quad\copyrightline\hfill\emph{Environmetrics}\ {\bfseries\volumeyear}, \volumenumber\ \startpage--\endpage\\
\sffamily\textit{Prepared using \textsf{envauth.cls}}}}
\fi
\ifreferee
\def\@oddfoot{\centerline{\thepage}}
\else
\def\@oddfoot{\parbox[t]{\textwidth}{
\noindent{\color{blue}\rule{\textwidth}{5.0pt}}\par
\sffamily\small \emph{Environmetrics }\ {\bfseries\volumeyear}, \volumenumber\ \startpage--\endpage\hfill\copyrightline\quad\ssboxnum\\
\sffamily\textit{Prepared using \textsf{envauth.cls}}}}
\fi
}

\let\@hangfrom\noindent %Remove indent on section turnovers
\renewcommand{\@seccntformat}[1]{{\csname the#1\endcsname.}\hspace{0.5em}}

\newdimen\@bls                              
\@bls=\baselineskip

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {1.5\@bls  plus .3\@bls minus .1\@bls}%
                                   {.5\@bls\@afterindentfalse}%
                                   {\normalfont\sectionsize\raggedright\bfseries\MakeUppercase}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {\@bls plus .3\@bls minus .1\@bls}%
                                     {6pt\@afterindentfalse}%
                                     {\normalfont\normalsize\raggedright\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {\@bls plus .3\@bls minus .1\@bls}%
                                     {6pt\@afterindentfalse}%
                                     {\normalfont\normalsize\raggedright\itshape}}
%\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
%                                     {\@bls plus .2\@bls}%
%                                     {-5pt}%
%                                     {\normalfont\normalsize\itshape}}

\def\enumerate{\ifnum \@enumdepth >3 \@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
        {\@enumctr}\itemsep 0pt\parsep 0pt
         \def\makelabel##1{\hss\llap{##1}}}\fi}

\let\endenumerate =\endlist

\def\itemize{\ifnum \@itemdepth >3 \@toodeep\else \advance\@itemdepth \@ne
\edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
\list{\csname\@itemitem\endcsname}{\itemsep 0pt\parsep 0pt
\def\makelabel##1{\hss\llap{##1}}}\fi}

\let\enditemize =\endlist

\usepackage[tableposition=top]{caption}
\DeclareCaptionLabelSeparator{jwperiod}{.\hspace*{0.5ex}}
\captionsetup[figure]{font=scriptsize,labelfont=bf,labelsep=jwperiod,justification=justified,singlelinecheck=true}
\captionsetup[table]{font=normalsize,labelfont=bf,labelsep=jwperiod,justification=centerlast,singlelinecheck=true}

\newcommand\ack{\subsection*{\sectionsize Acknowledgement}}
\newcommand\acks{\subsection*{\sectionsize Acknowledgements}}

\newif\ifAppsect%
\Appsectfalse%

\newif\ifappfig%
\appfigfalse%

\newif\ifapptab%
\apptabfalse%

\newcounter{appfigcnt}%
\newcounter{apptabcnt}%

\renewcommand{\thefigure}{\ifappfig\relax A.\fi\arabic{figure}}
\renewcommand{\thetable}{\ifapptab\relax A.\fi\arabic{table}}

\def\appendix{\section*{Appendix}%
\Appsecttrue%
\renewcommand{\thesection}{\Alph{section}}
\renewcommand{\theequation}{A.\arabic{equation}}
\setcounter{section}{0}
\setcounter{equation}{0}
\ifreferee%
\setcounter{postfig}{0}
\setcounter{posttbl}{0}
\else%
\appfigtrue%
\apptabtrue%
\setcounter{figure}{0}
\setcounter{table}{0}
\fi}

%\def\appendix{\section*{Appendix}\renewcommand{\thesection}{\Alph{section}}
%\setcounter{section}{0}}

\renewcommand\refname{REFERENCES}

\renewenvironment{thebibliography}[1]{%
\section*{\refname}
\list{{\arabic{enumi}}}{\def\makelabel##1{\hss{##1.}}\topsep=0\p@\parsep=0\p@
\partopsep=0\p@\itemsep=0\p@
\labelsep=1ex\itemindent=0\p@
\settowidth\labelwidth{\small[#1]}%
\leftmargin\labelwidth
\advance\leftmargin\labelsep
\advance\leftmargin -\itemindent
\usecounter{enumi}}\small
\def\newblock{\ }
\sloppy\clubpenalty4000\widowpenalty4000
\sfcode`\.=1000\relax
}{\endlist}

\def\biog{\section*{Author's Biography}\small}
\def\biogs{\section*{Authors' Biographies}\small}

\AtBeginDocument{
\setlength{\mathindent}{0pt}
\setlength{\bibsep}{0pt}
\ifreferee
\usepackage[nolists,nofighead,notabhead,tabhead,fighead]{endfloat}
\usepackage{chngendfloat}
\def\bibfont{\fontsize{10}{12}\selectfont}
\else
\def\bibfont{\fontsize{8}{12}\selectfont}
\fi
}

\AtEndDocument{%
\label{LastPage}}

\newenvironment{proof}[1][Proof]{\par
  \normalfont \topsep7\p@\@plus6\p@\relax
  \noindent{\itshape #1.}\ \ %\newline%\itshape
}{%
%\popQED
%\hfill\resizebox{6pt}{!}{\fbox{\fontsize{0.1}{0.1}\selectfont\phantom{a}}}
\endtrivlist\@endpefalse
}

\hyphenation{com-mu-ni-ca-tions}

\pagestyle{env}
\normalsize
\sloppy

