%%%%%%%%%%%%%%  lhs2TeX Color-related things   %%%%%%%%%%%%%%

\usepackage[dvipsnames]{xcolor}

%include polycode.fmt

%subst keyword a = "\textcolor{BlueViolet}{\textbf{" a "}}"

\newcommand{\id}[1]{\textsf{\textsl{#1}}}

\renewcommand{\Varid}[1]{\textcolor{Sepia}{\id{#1}}}
\renewcommand{\Conid}[1]{\textcolor{OliveGreen}{\id{#1}}}

%%%%%%%%%%%%  End of lhs2TeX Color-related things   %%%%%%%%%%%%

% It might make sence to add pretty formating of individual things 
% like "forall", cf.
% https://github.com/goldfirere/thesis/blob/master/tex/rae.fmt
