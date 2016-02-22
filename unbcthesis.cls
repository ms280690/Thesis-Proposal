% -------------------------------------------------

% unbcthesis.cls

%

% This sty file is for use in creating UNBC thesis 
% documents. It allows for easy creation of title 
% pages, tables of content, and Copyright forms 
% formatted to the UNBC Office of Graduate Programs 
% guidelines when using pdflatex.exe contained in 
% MikTex 2.8. It is also designed to make it easy to 
% include images that are designed to be used for
% pdfTEX creation i.e. pdf, png, and jpg

%

% This LaTeX style is heavily based on the work of
% Donald Craig (donald@mun.ca) at the Memorial 
% University of Newfoundland, Newfoundland and Labrador, 
% Canada. Thanks.

% 

% Created by Allan Kranz Senior Lab Instuctor
% Computer Science, UNBC

% Converted from unbcthesis.sty to unbcthesis.cls by
% David Casperson, based on version of
% Allan Kranz Senior Lab Instuctor
% Computer Science, UNBC

% -------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -- Basics                                                       -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{unbcthesis}[2015/10/24 UNBC Thesis Style]
%                          %Y/%m/%d


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Packages                                                    -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{graphicx}
\RequirePackage{endnotes}
\RequirePackage{calc}


%%==========================================================%%
%% --  Font Packages                                     -- %%
%%=========================================================%%%
% Try uncommenting the following line if fonts don't

% display correctly in the PDF file.

%\RequirePackage{pslatex} 



% To use palatino font, uncomment the following line.

%\RequirePackage{mathpazo}



% To use times roman font, uncomment the following line.

\RequirePackage{mathptmx}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Class Options                                               -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newif\ifDraft
\Draftfalse
\DeclareOption{final}{\Draftfalse}
\DeclareOption{nofinal}{\Drafttrue}
\DeclareOption*{\PassOptionsToPackage{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass{report}
%% packages dependent on class and options
\RequirePackage[doublespacing]{setspace}
\RequirePackage[plainpages=false,
  pdfpagelabels]{hyperref}  % Get nice PDF links



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Section Formatting                                          -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

%+ from ClemsonProposal.cls
\newenvironment{frontmatter}
  {\pagestyle{plain}\pagenumbering{roman}}
  {\cleardoublepage\pagenumbering{arabic}}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  End note miscellanious                                      -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\elabel#1{\@bsphack
  \protected@write\@auxout{}%
         {\string\newlabel{#1}{{\theendnote}{\thepage}}}%
         \@esphack}
\def\eref#1{%
    \@ifundefined{r@#1}{\def\next{\def\theendnote{{??}}}}%
              {\xdef\next{%
               \expandafter\noexpand\csname c@endnote\endcsname%
              \expandafter\expandafter\expandafter\@firstoftwo\csname
                r@#1\endcsname\relax}}%
  \begingroup
  \next \xdef\@theenmark{\theendnote}%
  \endgroup\@endnotemark}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Formatting                                                  -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand\enoteformat{%
    \setlength{\parindent}{0em}%
    \leavevmode\noindent
    \llap{\hb@xt@1.5em{\@makeenmark\hss}}}
\def\@endanenote{\string\par\string\endgroup}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Spacing and Page Layout                                     -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The following stolen from Reviews/2015/Internal-Report/csreport.cls
%% in /Users/casper/University/Department
%% not yet incorporated

%+0- \setlength{\heavyrulewidth}{0.1em}
%+0- \setlength{\lightrulewidth}{0.05em}
%+0- 
%+0- \linespread{1.172}% 12pt / 17pt 
%+0- \setlength{\parindent}{0pt}
%+0- \setlength{\parskip}{1\baselineskip plus 1ex minus 1ex}
%+0- 
%+0- \setlength{\headheight}{17pt}
%+0- \setlength{\topmargin}{-1\headheight}
%+0- \setlength{\headsep}{0.5in}
%+0- \setlength{\oddsidemargin}{0.50in}
%+0- \setlength{\evensidemargin}{0.50in}
%+0- \setlength{\textwidth}{5.5in}
%+0- \setlength{\textheight}{8in}
%+0- \pagestyle{fancy}
%+0- \fancyhead{}
%+0- \headheight 17pt
%+0- \renewcommand\headrulewidth{0.5pt}
%+0- %%\renewcommand\footrulewidth{0pt}
%+0- \fancyfoot{}
%+0- \fancyhead[L]{\textsf{\color{gray}Computer Science Self-Study Document}}
%+0- \fancyhead[R]{{\sffamily{\textcolor{gray}{Page}}\hspace*{0.5em}\thepage}}
%+0- \thispagestyle{empty}
%+0- \setlength{\rightskip}{0pt plus 1em}
%+0- \setlength{\csname @rightskip\endcsname}{\rightskip}



%% The following modified from Kranz
\setlength{\oddsidemargin }{0.5in}
\setlength{\textwidth     }{6in}
\setlength{\headheight    }{0in}
\setlength{\headsep       }{0in}
\setlength{\topmargin     }{0in}
\setlength{\textheight    }{9in}
\setlength{\floatsep      }{0.29in}
\setlength{\textfloatsep  }{1\floatsep}
\setlength{\intextsep     }{1\floatsep}

% Almost everything is double spaced.
%

% Footnotes are to be single spaced.
%
\newcommand{\unbcfootnote}[1]{%
	\begin{singlespace}
	\footnote{#1}%
        \end{singlespace}
}

% Lengthy quotes must also be single spaced.
% We provide a parameter for the citation label too.
%
\newenvironment{unbcquote}{%
	\begin{singlespace}
	\par\vspace{1.5mm}%
	\begin{quote}\slshape
        }{%
	\end{quote}%
        \end{singlespace}
}

% Command for creating the thesis table of contents page
% in accordance with the requirements of the UNBC Office 
% of Graduate Programs.
%

\renewcommand*\l@section{\@dottedtocline{1}{1.5em}{2.8em}}
\renewcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\renewcommand*\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\renewcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.8em}}
\let\l@table\l@figure

\newcommand{\unbctableofcontents}{
  \renewcommand{\contentsname}{\center{\small TABLE OF CONTENTS}}%
  \phantomsection
  \addcontentsline{toc}{chapter}{Table of Contents}%
  \begin{singlespacing}
  \tableofcontents
  \end{singlespacing}
}

% Command for creating the thesis list of tables page
% in accordance with the requirements of the UNBC Office 
% of Graduate Programs.
%

\newcommand{\unbclistoftables}{
  \renewcommand{\listtablename}{\center{\small LIST OF TABLES}}%
  \phantomsection
  \addcontentsline{toc}{chapter}{List of Tables}%
  \begin{singlespacing}
  \listoftables
  \end{singlespacing}
}


% Command for creating the thesis list of figures page
% in accordance with the requirements of the UNBC Office 
% of Graduate Programs.
%

\newcommand{\unbclistoffigures}{
  \renewcommand{\listfigurename}{\center{\small LIST OF FIGURES}}%
  \phantomsection
  \addcontentsline{toc}{chapter}{List of Figures}%
  \begin{singlespacing}
  \listoffigures
  \end{singlespacing}
}

\newcommand{\unbclistofcode}{
  \phantomsection
  \addcontentsline{toc}{chapter}{List of Code Listings}%
  \begin{singlespacing}
  \listof{code-list}{\center{\small LIST OF CODE LISTINGS}}
  \end{singlespacing}
}

% The following commands are stolen from the ClemsonProposal.cls
\gdef\UNBCthesis@Month          {}
\gdef\UNBCthesis@Year           {}
\gdef\UNBCthesis@Major          {}
\gdef\UNBCthesis@Degree         {}
\gdef\UNBCthesis@MemberI        {}
\gdef\UNBCthesis@MemberII       {}
\gdef\UNBCthesis@MemberIII      {}
\gdef\UNBCthesis@MemberIV       {}
\gdef\UNBCthesis@MemberV        {}
\gdef\UNBCthesis@MemberVI       {}
\gdef\UNBCthesis@Author         {}
\gdef\UNBCthesis@Title          {}

\newcommand{\setMonth}    [1]{\ifx\empty#1\else \gdef\UNBCthesis@Month    {#1}\fi}
\newcommand{\setYear}     [1]{\ifx\empty#1\else \gdef\UNBCthesis@Year     {#1}\fi}
\newcommand{\setMajor}    [1]{\ifx\empty#1\else \gdef\UNBCthesis@Major    {#1}\fi}
\newcommand{\setDegree}   [1]{\ifx\empty#1\else \gdef\UNBCthesis@Degree   {#1}\fi}
\newcommand{\setMemberI}  [1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberI  {#1}\fi}
\newcommand{\setMemberII} [1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberII {#1}\fi}
\newcommand{\setMemberIII}[1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberIII{#1}\fi}
\newcommand{\setMemberIV} [1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberIV {#1}\fi}
\newcommand{\setMemberV}  [1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberV  {#1}\fi}
\newcommand{\setMemberVI} [1]{\ifx\empty#1\else \gdef\UNBCthesis@MemberVI {#1}\fi}
\newcommand{\setAuthor}   [1]{\ifx\empty#1\else \gdef\UNBCthesis@Author   {#1}\fi}
\newcommand{\setTitle}    [1]{\ifx\empty#1\else \gdef\UNBCthesis@Title    {#1}\fi}


\newcommand{\theMonth}    {\UNBCthesis@Month    }
\newcommand{\theYear}     {\UNBCthesis@Year     }
\newcommand{\theMajor}    {\UNBCthesis@Major    }
\newcommand{\theDegree}   {\UNBCthesis@Degree   }
\newcommand{\theMemberI}  {\UNBCthesis@MemberI  }
\newcommand{\theMemberII} {\UNBCthesis@MemberII }
\newcommand{\theMemberIII}{\UNBCthesis@MemberIII}
\newcommand{\theMemberIV} {\UNBCthesis@MemberIV }
\newcommand{\theMemberV}  {\UNBCthesis@MemberV  }
\newcommand{\theMemberVI} {\UNBCthesis@MemberVI }
\newcommand{\theAuthor}   {\UNBCthesis@Author   }
\newcommand{\theTitle}    {\UNBCthesis@Title    }


% Command for creating the thesis title page in accordance with the
% requirements of the UNBC Office of Graduate Programs.
%
\newcommand{\unbcthesistitle}[7]{%
  %% Parameters
  %  #1 The title
  %  #2 The author
  %  #3 Credential, e.g., Master of Science
  %  #4 Degree area, e.g., Computer Science
  %  #5 Month of publication  (e.g., March)
  %  #6 Year  of publication  (e.g., 2015)
  %  #7 Previous degrees, e.g., 
  %  - B.Sc., University of Northern British Columbia, 2013
  \thispagestyle{empty}%
  \begin{center}%
    \begin{singlespace}
    \vspace{1pc}%
    \textbf{#1}
    \end{singlespace}
    \vspace{2pc}
    by \\
    \vspace{2pc}
    \textbf{#2} \\
    \vspace{1pc}
    #7 \\
    \vspace{11pc}

    \begin{singlespace}
    THESIS SUBMITTED IN PARTIAL FULFILLMENT OF \\
    THE REQUIREMENTS FOR THE DEGREE OF \\
    \MakeUppercase {#3} \\
    IN \\
    \MakeUppercase {#4} \\
    \end{singlespace}
    
    \vspace{11pc}

    UNIVERSITY OF NORTHERN BRITISH COLUMBIA \\
    \vspace{1pc}
    #5 #6 \\
    \vspace{2pc}

    \copyright\ #2, #6 
  \end{center}
\vspace{\fill}
}

%   ------------------------------------------------------------   %

\newenvironment{unbcabstract}{
    \begin{center}
        \Large{\textbf{Abstract}}
        \vspace{.1in}
    \end{center}
    \phantomsection
    \addcontentsline{toc}{chapter}{Abstract}
}{\par\newpage}


%   ------------------------------------------------------------   %

% Command for producing a bibliography in UNBC approved style

\newcommand\unbcbibliography[1]{
    \begin{singlespacing}
    \phantomsection
    \addcontentsline{toc}{chapter}{Bibliography}
    \bibliography{#1}
    \end{singlespacing}
}

%   ------------------------------------------------------------   %

% Command for creating the thesis copyright page in accordance with the
% requirements of the UNBC Office of Graduate Programs.
%
% Usage:
%
% Fill in the title, author, month, and year.

% \unbcthesiscopyright
%	{Digital Filtering Using a Neural Network}
%       {Allan Edward Kranz}
%       {10 May 2010}
%
\newcommand{\unbcthesiscopyright}[3]{
    \newlength{\txtwdtha}%
    \newlength{\txtwdthb}%
    \newlength{\txtwdthc}%
    \newlength{\txtwdthd}%
    \newlength{\txtwdthe}%
    \settowidth{\txtwdtha}{#1}%
    \settowidth{\txtwdthb}{#2}%
    \settowidth{\txtwdthc}{#3}%
    \settowidth{\txtwdthd}{#3}%
    \settowidth{\txtwdthe}{#3}%



    \thispagestyle{empty}%
    \newpage
    \begin{center}
      \textbf{UNIVERSITY OF NORTHERN BRITISH COLUMBIA PARTIAL \\ 
        COPYRIGHT LICENCE}
    \end{center}

    
    I hereby grant the University of Northern British Columbia Library
    the right to lend my project/thesis/dissertation to users of the
    library or to other libraries.
    Furthermore, I grant the University of Northern British Columbia
    Library the right to make single copies only of my
    project/thesis/dissertation for users of the library or in response
    to a request from other libraries, on their behalf or for one of
    their users.
    Permission for extensive copying of this project/thesis/dissertation
    for scholarly purposes may be granted by me or by a member of the
    university designated by me.
    It is understood that copying or publication of this
    thesis/dissertation for financial gain shall not be allowed without
    my written permission.

    \vspace{3pc}

    \noindent 
    Title of Project/Thesis/Dissertation:
    \rule[-0.1cm]{\linewidth-\txtwdthd}{0.01cm}\\

    \noindent
    \rule[-0.1cm]{\linewidth*\real{0.5}-\txtwdtha*\real{0.5}}{0.01cm}
    #1
    \rule[-0.1cm]{\linewidth*\real{0.5}-\txtwdtha*\real{0.5}}{0.01cm} \\

    \noindent
    \rule[-0.1cm]{\linewidth}{0.01cm} \\

    \vspace{1pc}

    \noindent Author 	
    \rule[-0.1cm]{((\linewidth-\txtwdthe)*\real{0.3}-\txtwdthb)*\real{0.5}}{0.01cm}
    #2
    \rule[-0.1cm]{((\linewidth-\txtwdthe)*\real{0.3}-\txtwdthb)*\real{0.5}}{0.01cm}

    \rule[-0.1cm]{(\linewidth-\txtwdthe)*\real{0.3}}{0.01cm}
    \rule[-0.1cm]{((\linewidth-\txtwdthe)*\real{0.3}-\txtwdthc)*\real{0.5}}{0.01cm}
    #3
    \rule[-0.1cm]{((\linewidth-\txtwdthe)*\real{0.3}-\txtwdthc)*\real{0.5}}{0.01cm}\\
    \hspace*{25mm}Printed Name \hspace*{25mm}Signature \hspace*{25mm}Date 
    \vspace{\fill}
}

%   ------------------------------------------------------------   %

% The \unbcfig command is used to insert a new 
% pdf, png, or jpg figure into our document.  
% Usage is:

%
%	\unbcfig[args]{filename}{caption}
%

% where:
%	- the optional 'args' argument is passed to the
%	  embedded \includegraphics command, this can be used
%	  to scale the figure or rotate it.

%	- 'filename' is the name of the graphics file in the 'figures'
%	  directory that is to be inserted (note that 'filename'
%         should include the  extension).
%	- 'filename' also serves as the label for the figure.
%	  with the text 'fig:' prepended.
%

% Sample Usage:

% 	\unbcfig[scale=0.5,angle=90]{barchart.png}{Population over time}

% inserts the graphic file 'figures/barchart.png' reduced in size by 50%
% rotated 90 degrees and with the caption "Popuation over Time."
% We can refer to that figure as Figure~\ref{fig:barchart} in the text.
%

\newcommand{\unbcfig}[3][scale=1.0]{%
	\begin{figure}[!htbp]%
		\centering
		\vspace{2mm}%
		\includegraphics[#1]{figures/#2}%
		\caption{#3}%
		\label{fig:#2}%
	\end{figure}
}

%   ------------------------------------------------------------   %

% \unbclfig command inserts a figure in landscape mode.  The
% entire page is rotated to accommodate the figure.  The arguments
% are the same as for \unbcfig, above
%

\newcommand{\unbclfig}[3][scale=1.0]{%
	\begin{sidewaysfigure}[!htbp]
		\centering
		\vspace{2mm}%
		\includegraphics[#1]{figures/#2}%
		\caption{#3}%
		\label{fig:#2}%
	\end{sidewaysfigure}
}

%   ------------------------------------------------------------   %

% The 'unbctxtfig' environment is used to insert 'textual' figures
% into our document, such as brief source code snippets.  Usage is:
%

%	\begin{unbctxtfig}[spacing]{label}{caption}{width}
%	  text for 'figure'
%	\end{unbctxtfig}
%
% where:
%	- 'spacing' is a number representing the baselinestretch
%	  (line-spacing) to use for the text figure.  Default is
%          single-spacing.

%	- 'label' is the label to be used for referencing.
%	   The figure can be referenced as Figure~\ref{fig:label}.

%	- 'caption' is the caption to display below the figure.

%	- 'width' is the width of the minipage in which the text figure
%	  is formatted.

%

% Sample usage:
% \begin{unbctxtfig}[1.0]{code}{Hello World}{0.5\textwidth}
%   Some text
% \end{unbctxtfig}
%

\newenvironment{unbctxtfig}[4][1.0]{%
	\begin{figure}[!htbp]
		\centering
		\def\unbccaption{#3}%
		\def\unbclabel{#2}%
		\begin{spacing}{#1}
		\begin{minipage}{#4}
		\hrule \hrule
		\bigskip
}{%

		\hrule \hrule
		\end{minipage}%
                \end{spacing}
		\caption{\unbccaption}%
		\label{fig:\unbclabel}%
	\end{figure}
}

%   ------------------------------------------------------------   %

% The 'unbctab' environment is used to insert a new table into our document.
% Usage is:

%
%	\begin{unbctab}{table_format}{label}{caption}
%            table contents
%       \end{unbctab}
%

% where:
%	- 'label' is the label used to reference the Table.
%	   We can refer to the table as Table~\ref{tab:label} in
%          the text.

%	- 'caption' is the caption placed above the Table.
%
%

\newenvironment{unbctab}[3]{%
	\begin{table}[!htbp]%
		\centering
		\caption{#3}%
		\label{tab:#2}%
		\vspace{2mm}%
		\begin{tabular}{#1}%

}{%

		\end{tabular}
	\end{table}
}

%   ------------------------------------------------------------   %

% The 'unbcltab' environment is like unbctab, but displays the table
% in landscape mode on its own page.  See unbctab environment for usage.
%

\newenvironment{unbcltab}[3]{%
	\begin{sidewaystable}
		\centering
		\caption{#3}%
		\label{tab:#2}%
		\vspace{2mm}%
		\begin{tabular}{#1}%

}{%
		\end{tabular}%
	\end{sidewaystable}%
}

%   ------------------------------------------------------------   %

% The \unbceqn environment is used to add an equation to the thesis.
% Usage is:
%
%	\begin{unbceqn}{label}
%            equation body
%	\end{unbceqn}

%
% where:
%	- 'label' is the label used to reference the Equation.
%	   We can refer to the equation as Table~\ref{eqn:label} in
%          the text.
%

\newenvironment{unbceqn}[1]{%
	\begin{equation}%
		\label{eqn:#1}
}{%
	\end{equation}
}

%   ------------------------------------------------------------   %



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --  Unincorporated Grot                                         -- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% To have the references sorted in the order they are cited,
% (instead of alphabetically), use 'unsrt' instead of 'abbrv'.
%

\newenvironment{scope}{}{}
\bibliographystyle{amsplain}
