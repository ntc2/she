\documentclass{beamer}

\definecolor{dred}{rgb}{0.5,0,0}
\definecolor{dgreen}{rgb}{0,0.5,0}
\definecolor{dblue}{rgb}{0,0,0.5}
\definecolor{dorange}{rgb}{0.9,0.25,0}
\definecolor{dbrown}{rgb}{0.3,0.1,0}
\definecolor{lred}{rgb}{1,0.6,0.6}
\definecolor{lgreen}{rgb}{0.6,1,0.6}
\definecolor{lblue}{rgb}{0.6,0.6,1}
\definecolor{lgrey}{rgb}{0.8,0.8,0.8}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt
\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}


%if False

> type Unit = ()
> pattern Void = ()

%endif

\newcommand{\T}[1]{{\color{dblue}{#1}}}
\newcommand{\C}[1]{{\color{dred}{#1}}}
\newcommand{\F}[1]{{\color{dgreen}{#1}}}
\newcommand{\K}[1]{{\color{black}{#1}}}
\newcommand{\Cl}[1]{{\color{dbrown}{#1}}}
\newcommand{\lT}[1]{{\color{lblue}{#1}}}
\newcommand{\lC}[1]{{\color{lred}{#1}}}
\newcommand{\lF}[1]{{\color{lgreen}{#1}}}
\newcommand{\lK}[1]{{\color{lgrey}{#1}}}

%format Unit = "\T{()}"
%format Void = "\T{()}"

%subst keyword a = "\mathkw{" a "}"
%subst conid a = "\mathsf{" a "}"

\title{\color{dorange}{The {\color{dbrown}{S}}trathclyde {\color{dbrown}{H}}askell {\color{dbrown}{E}}nhancement}}
\author{Conor McBride\qquad\qquad\qquad\qquad}
\date{\today\qquad\qquad\qquad\qquad}
\begin{document}

\begin{frame}
  \titlepage
\vspace*{-3cm}
\hfill\includegraphics[height=5cm]{term-and-type.jpg}
\end{frame}

\begin{frame}{A Break with Tradition?}

\begin{itemize}
\item<2->\color{dblue}{I'm going to use a computer.}
\item<3->\color{dblue}{I'm going to use Haskell.}
\item<4->\F{I'm going to run a program.}
\end{itemize}
\begin{itemize}
\item<5->\color{dred}{What have I done with the real Conor?}
\end{itemize}
\end{frame}

\begin{frame}{It starts like this...}

\uncover<2->{\color{dbrown}{

> {-# OPTIONS\_GHC -F -pgmF she#-}

}}
\uncover<3->{
\color{black}{...and this ain't looking much better...}
}
\uncover<4->{
\color{dbrown}{

> {-# LANGUAGE KindSignatures, RankNTypes #-}
> {-# LANGUAGE TypeOperators, GADTs #-}
> {-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

}}
\uncover<5->{
\color{black}{...but this...}
}
\uncover<6->{
\color{dbrown}{

> module FileDemo where

> import System.FilePath
> import System.IO
> import System.IO.Error

}}
\uncover<7->{
\color{black}{...suggests that we might even do something.}
}
\end{frame}

\begin{frame}{(Monkey) Business as usual}
\uncover<2->{\color{dbrown}{

> import ShePrelude  -- voodoo
> import IFunctor    -- second-order jiggery-pokery
> import IMonad      -- third-order jiggery-pokery-transformers

}}
\end{frame}

\begin{frame}{File Handles with State}

%format State = "\mathsf{\T{State}}"
%format FilePath = "\mathsf{\T{FilePath}}"
%format Char = "\mathsf{\T{Char}}"
%format Maybe = "\mathsf{\T{Maybe}}"
%format Open = "\mathsf{\C{Open}}"
%format Closed = "\mathsf{\C{Closed}}"
%format where = "\;\mathkw{where}"

%format :+: = "\mathbin{\T{:\!\!+\!\!:}}"
%format :- = "\mathbin{\T{:\!\!-\!\!}}"
%format :>>: = "\mathbin{\T{:\!>\!\!>\!\!:}}"
%format FH = "\mathsf{\T{FH}}"

> data State :: * where
>   Open    :: State
>   Closed  :: State
>   deriving SheSingleton -- \emph{what's that?}

\uncover<2->{
and a choice of operations.

> type FH -- |:: ({State} -> *) -> {State} -> *|
>   =    FilePath  :- {Closed}  :>>: (:: State)            -- fOpen
>   :+:  Unit      :- {Open}    :>>: Maybe Char :- {Open}  -- fGetC
>   :+:  Unit      :- {Open}    :>>: Unit :- {Closed}      -- fClose

}

\uncover<3->{
hint: \emph{`precondition'} |:>>:| \emph{`postcondition'}\\
}
\uncover<4->{
hint: \emph{thingIHave} |:-{| \emph{stateI'mIn}|}| ~ ~ (some data, some logic)\\
}
\uncover<5->{
hint: |(::State)| ~ ~ means `is a |State| known at run time'
}
\end{frame}

\begin{frame}{I fiddle about in the back of the room,...}
%format pattern = "\mathkw{pattern}"
%format FOpen = "\mathsf{\C{FOpen}}"
%format FClose = "\mathsf{\C{FClose}}"
%format FGetC = "\mathsf{\C{FGetC}}"
%format Do = "\mathsf{\C{Do}}"
%format Ret = "\mathsf{\C{Ret}}"
%format :* = "\mathbin{\T{{}^{:\ast}}}"
%format :& = "\mathbin{\C{:\!\!\&}}"
%format InL = "\mathsf{\C{InL}}"
%format InR = "\mathsf{\C{InR}}"
%format V = "\mathsf{\C{V}}"

> pattern FOpen p  k = Do (InL (V p :& k))
> pattern FGetC    k = Do (InR (InL (V Void :& k)))
> pattern FClose   k = Do (InR (InR (V Void :& k)))

(|pattern| synonyms are linear constructor-form definitions you can
use on either side of your program)

%format fOpen = "\mathsf{\F{fOpen}}"
%format fGetC = "\mathsf{\F{fGetC}}"
%format fClose = "\mathsf{\F{fClose}}"

> fOpen    ::  FilePath -> (FH :* (:: State)) {Closed}
> fOpen p  =   FOpen p Ret
> fGetC    ::  (FH :* (Maybe Char :- {Open})) {Open}
> fGetC    =   FGetC Ret
> fClose   ::  (FH :* (Unit :- {Closed})) {Open}
> fClose   =   FClose Ret

|Ret| and |Do| are the constructors of |:*|, as we'll see in a bit.
\end{frame}

\begin{frame}{...I write an interpreter,...}
%format runFH = "\mathsf{\F{runFH}}"
%format openFH = "\mathsf{\F{openFH}}"
%format return = "\mathsf{\F{return}}"
%format catch = "\mathsf{\F{catch}}"
%format hClose = "\mathsf{\F{hClose}}"
%format openFile = "\mathsf{\F{openFile}}"
%format hGetChar = "\mathsf{\F{hGetChar}}"
%format IO = "\mathsf{\T{IO}}"
%format Handle = "\mathsf{\T{Handle}}"
%format Just = "\mathsf{\C{Just}}"
%format Nothing = "\mathsf{\C{Nothing}}"
%format ReadMode = "\mathsf{\C{ReadMode}}"
%format >>= = "\mathbin{\F{>\!\!\!>\!\!=}}"
%format >> = "\mathbin{\F{>\!\!\!>}}"

> runFH :: (FH :* (a :- {Closed})) {Closed} -> IO a
> runFH (Ret (V a)) = return a
> runFH (FOpen s k) = catch
>   (openFile s ReadMode >>= openFH (k {Open}))
>   (\ _ -> runFH (k {Closed}))
>   where
>     openFH :: (FH :* (a :- {Closed})) {Open} -> Handle -> IO a
>     openFH (FClose  k) h = hClose h >> runFH (k (V Void))
>     openFH (FGetC   k) h = catch
>       (hGetChar h >>= \ c -> openFH (k (V (Just c))) h)
>       (\ _ -> openFH (k (V Nothing)) h)

\end{frame}

\begin{frame}{...and then I write a little program,...}
%format fileContents = "\mathsf{\F{fileContents}}"
%format readOpenFile = "\mathsf{\F{readOpenFile}}"
%format ?>= = "\mathbin{\F{?\!\!\!>\!\!=}}"
%format =>= = "\mathbin{\F{=\!\!\!>\!\!=}}"
%format : = "\mathbin{\C{:}}"
%format String = "\mathsf{\T{String}}"

%if False

> nil = ""

%endif
%format nil = "{\C{\mbox{``''''}}}"

> fileContents ::  FilePath ->
>                  (FH :* (Maybe String :- {Closed})) {Closed}
> fileContents p = fOpen p ?>= \ s -> case s of
>   {Closed}  -> (| Nothing |)
>   {Open}    -> (| Just readOpenFile (-fClose-) |)

> readOpenFile :: (FH :* (String :- {Open})) {Open}
> readOpenFile = fGetC =>= \ x -> case x of
>   Nothing  ->  (| nil |)
>   Just c   ->  (| ~c : readOpenFile |)

...but is it Haskell?

\end{frame}

\begin{frame}{How about I run this program?}
\begin{block}{}
I suppose that means I should suspend Preview and run ghci, in some
sort of emacs buffer.
\end{block}
\end{frame}


\begin{frame}{What's Going On?}
\begin{itemize}
\item<2->\color{dblue}{Dependent types?}
\item<3->\color{dblue}{Macros?}
\item<4->\color{dblue}{A new kind of monad?}
\end{itemize}

\uncover<5->{Yeah}\uncover<6->{, but no, but it is...}

\uncover<7->{...the \emph{\color{dorange}{Strathclyde Haskell Enhancement}}!}
\end{frame}

\begin{frame}{Bizarre Brackets in Peculiar Places}

\begin{block}{}
Let's see that again...
\end{block}

< fileContents ::  FilePath ->
<                  (FH :* (Maybe String :- {Closed})) {Closed}
< fileContents p = fOpen p ?>= \ s -> case s of
<   {Closed}  -> (| Nothing |)
<   {Open}    -> (| Just readOpenFile (-fClose-) |)

< readOpenFile :: (FH :* (String :- {Open})) {Open}
< readOpenFile = fGetC =>= \ x -> case x of
<   Nothing  ->  (| nil |)
<   Just c   ->  (| ~c : readOpenFile |)

\end{frame}

\definecolor{hilite}{rgb}{0,0,0}
\begin{frame}{Bizarre Brackets in Peculiar Places}

\begin{block}{}
Let's see that again... braces in types.
\end{block}

%format Br x = "{\color{hilite}{\{}}" x "{\color{hilite}{\}}}"
%format (Br x) = "{\color{hilite}{\{}}" x "{\color{hilite}{\}}}"

%format fOpen' = "\mathsf{\lF{fOpen}}"
%format fGetC' = "\mathsf{\lF{fGetC}}"
%format fClose' = "\mathsf{\lF{fClose}}"
%format fileContents' = "\mathsf{\lF{fileContents}}"
%format readOpenFile' = "\mathsf{\lF{readOpenFile}}"
%format ?>=! = "\mathbin{\lF{?\!\!\!>\!\!=}}"
%format =>=! = "\mathbin{\lF{=\!\!\!>\!\!=}}"
%format :! = "\mathbin{\lC{:}}"
%format :*! = "\mathbin{\lT{:*}}"
%format :-! = "\mathbin{\lT{:\!\!-\!\!}}"
%format String' = "\mathsf{\lT{String}}"
%format Just' = "\mathsf{\lC{Just}}"
%format Nothing' = "\mathsf{\lC{Nothing}}"
%format FilePath' = "\mathsf{\lT{FilePath}}"
%format FH' = "\mathsf{\lT{FH}}"
%format Char' = "\mathsf{\lT{Char}}"
%format Maybe' = "\mathsf{\lT{Maybe}}"
%format Open' = "\mathsf{\lC{Open}}"
%format Closed' = "\mathsf{\lC{Closed}}"
%format nil' = "{\lC{\mbox{``''''}}}"
{\color{lgrey}{

< fileContents' ::  FilePath' ->
<                   (FH' :*! (Maybe' String' :-! Br Closed')) (Br Closed')
< fileContents' p = fOpen' p ?>=! \ s -> case s of
<   {Closed'}  -> (| Nothing' |)
<   {Open'}    -> (| Just' readOpenFile' (-fClose'-) |)

< readOpenFile' :: (FH' :*! (String' :-! Br Open')) (Br Open')
< readOpenFile' = fGetC' =>=! \ x -> case x of
<   Nothing'  ->  (| nil' |)
<   Just' c   ->  (| ~c :! readOpenFile' |)

}}


\end{frame}

\begin{frame}{Bizarre Brackets in Peculiar Places}
\begin{block}{}
Let's see that again... braces around patterns (and expressions)
\end{block}

{\color{lgrey}{

< fileContents' ::  FilePath' ->
<                   (FH' :*! (Maybe' String' :-! {Closed'})) {Closed'}
< fileContents' p = fOpen' p ?>=! \ s -> case s of
<   Br Closed'  -> (| Nothing' |)
<   Br Open'    -> (| Just' readOpenFile' (-fClose'-) |)

< readOpenFile' :: (FH' :*! (String' :-! {Open'})) {Open'}
< readOpenFile' = fGetC' =>=! \ x -> case x of
<   Nothing'  ->  (| nil' |)
<   Just' c   ->  (| ~c :! readOpenFile' |)

}}
\end{frame}

%format nana (x) = "{\color{black}{(|}}" x "{\color{black}{|)}}"

\begin{frame}{Bizarre Brackets in Peculiar Places}
\begin{block}{}
Let's see that again... banana brackets
\end{block}

{\color{lgrey}{

< fileContents' ::  FilePath' ->
<                   (FH' :*! (Maybe' String' :-! {Closed'})) {Closed'}
< fileContents' p = fOpen' p ?>=! \ s -> case s of
<   {Closed'}  -> nana ( Nothing' )
<   {Open'}    -> nana ( Just' readOpenFile' (-fClose'-) )

< readOpenFile' :: (FH' :*! (String' :-! {Open'})) {Open'}
< readOpenFile' = fGetC' =>=! \ x -> case x of
<   Nothing'  ->  nana ( nil' )
<   Just' c   ->  nana ( ~c :! readOpenFile' )

}}
\end{frame}

%format (tack (x)) = "{\color{black}{(-}}" x "{\color{black}{-)}}"

\begin{frame}{Bizarre Brackets in Peculiar Places}
\begin{block}{}
Let's see that again... banana brackets with tack brackets inside
\end{block}

{\color{lgrey}{

< fileContents' ::  FilePath' ->
<                   (FH' :*! (Maybe' String' :-! {Closed'})) {Closed'}
< fileContents' p = fOpen' p ?>=! \ s -> case s of
<   {Closed'}  -> (| Nothing' |)
<   {Open'}    -> (| Just' readOpenFile' (tack (fClose')) |)

< readOpenFile' :: (FH' :*! (String' :-! {Open'})) {Open'}
< readOpenFile' = fGetC' =>=! \ x -> case x of
<   Nothing'  ->  (| nil' |)
<   Just' c   ->  (| ~c :! readOpenFile' |)

}}
\end{frame}

\begin{frame}{The Braces of Upward Mobility}
\includegraphics[height=8cm]{winging17.jpg}
\end{frame}

\begin{frame}{The Braces of Upward Mobility}
\includegraphics[height=8cm]{winging18.jpg}
\end{frame}

%format tau = "\tau"
%format kap = "\kappa"
%format forall = "\forall"
%format . = "\:.\:"

\newcommand{\SHE}{{\color{dbrown}{SHE}}}

\begin{frame}{Synchronized Swimming from Above and Below}
\begin{itemize}
\item<2->types like |State| become kinds like |{State}|
\item<6->\qquad but \SHE{} turns |{tau}| into |*|
\item<3->constructor forms (made from constructors and variables),
  like |Open|, move up to the type-level, with |{Open}::{State}|
\item<7->\qquad but \SHE{} declares \T{|SheTyOpen|} and
  maps |{Open}| to it
\item<4->you can now make |State|-indexed GADTs of kind |{State} -> *|
\item<8->\qquad but \SHE{} knows they're really of kind |* -> *|
\item<5->you can even make GADTs with \emph{polymorphic} kinds

< data (:- ) :: forall (x :: *). * -> {x} -> {x} -> * where
<   V :: a -> (a :- {k}) {k}

\item<9->\qquad but \SHE{} erases |forall (x :: kap).|
\uncover<10->(\emph{n.b.}, |x| occurs only in |{..}|
\end{itemize}
\end{frame}

\begin{frame}{Indexed Sets, Data as Witnesses}

\begin{block}{}
What does a kind like |{State} -> *| contain?\\
\uncover<2->{\qquad Sets \emph{indexed by} |State|s,}\\
\uncover<3->{\qquad capturing \emph{properties} of |State|s,}\\
\uncover<4->{\qquad containing data \emph{relevant} to a given |State|.}\\
~\\
\uncover<5->{Data carry significant \C{dynamic} information \emph{and}
  witness properties of their \T{static} index.}
\uncover<6->{

< data (:- ) :: forall (x :: *). * -> {x} -> {x} -> * where
<   V :: a -> (a :- {k}) {k}

|(a :- {k}) :: {x} -> *| (pronounced ``|a| atkey |k|'') carries values in
|a| at the \emph{key} index |k|, and is \emph{empty} at other indices.
}
\end{block}

\end{frame}

\begin{frame}{Or, to put it another way,}

\includegraphics[height=8cm]{negs.jpg}
\end{frame}

\begin{frame}{An Old Favourite}
%format Nat = "\mathsf{\T{Nat}}"
%format Z = "\mathsf{\C{Z}}"
%format S = "\mathsf{\C{S}}"
%format Vec = "\mathsf{\T{Vec}}"
%format Nil = "\mathsf{\C{Nil}}"
%format Cons = "\mathsf{\C{Cons}}"
%format vmap = "\mathsf{\F{vmap}}"
%format :-> = "\mathbin{\T{:\!\rightarrow}}"
> data Nat :: * where
>   Z :: Nat
>   S :: Nat -> Nat
>
> data Vec :: * -> {Nat} -> * where
>   Nil   :: Vec a {Z}
>   Cons  :: a -> Vec a {n} -> Vec a {S n}

\uncover<2->{\vspace*{-0.4in}

> type s :-> t = forall i. s {i} -> t {i}

\vspace*{-0.4in}}
\only<1-2>{

> vmap :: (a -> b) -> Vec a {n} -> Vec b {n}
> vmap f Nil          = Nil
> vmap f (Cons a as)  = Cons (f a) vmap f as

}\only<3->{

> vmap :: (a -> b) -> Vec a :-> Vec b
> vmap f Nil          = Nil
> vmap f (Cons a as)  = Cons (f a) vmap f as

}
\end{frame}

\begin{frame}{A New Favourite \emph{(reflexive-transitive closure)}}
%format Path = "\mathsf{\T{Path}}"
%format Stop = "\mathsf{\C{Stop}}"
%format :-: = "\mathbin{\C{:\!\!-\!\!:}}"
%format imap = "\mathsf{\F{imap}}"
%format sig = "\sigma"

> data Path :: ({i, i} -> *) -> {i, i} -> * where
>   Stop   :: Path sig {i, i}
>   (:-:)  :: sig {i, j} -> Path sig {j, k} -> Path sig {i, k}

You can write |{i,j}| for |{(i,j)}|, and |{}| for |{()}|.

\uncover<2->{%
An index- (\emph{i.e.}, endpoint-) respecting function on steps
induces an index-respecting map on paths.

< imap :: (sig :-> tau) -> Path sig :-> Path tau
< imap f Stop        = Stop
< imap f (s :-: ss)  = f s :-: imap f ss

}
\end{frame}

%format Vec' = "\mathsf{\T{Vec''}}"
%format Cons' = "\mathsf{\C{Cons''}}"
%format Monad = "\mathsf{\Cl{Monad}}"
%format Functor = "\mathsf{\Cl{Functor}}"
%format IFunctor = "\mathsf{\Cl{IFunctor}}"
%format phi = "\phi"
%format alf = "\alpha"

\begin{frame}{Nostrils twitching? \uncover<2->{They should be...}}
\uncover<3->{

> class IFunctor (phi :: ({i} -> *) -> {o} -> *) where
>   imap :: (sig :-> tau) -> phi sig :-> phi tau
>
> instance IFunctor Path where
>   imap f Stop        = Stop
>   imap f (r :-: rs)  = f r :-: imap f rs

}\uncover<4->{%
Make |Vec| an |IFunctor| by the power of \emph{one}...

< data Vec' :: ({} -> *) -> {Nat} -> * where
<   Nil    :: Vec' alf {Z}
<   Cons'  :: alf {} -> Vec alf {n} -> Vec alf {S n}
<
< instance IFunctor Vec' where
<   imap f Nil           = Nil
<   imap f (Cons' a as)  = Cons' (f a) vmap f as

}\uncover<5->{%
... and atkey back to where you were.

< type     Vec a {n}  = Vec' (a :- {}) {n}
< pattern  Cons a as  = Cons' (V a) as

}
\end{frame}

\begin{frame}{No Invention Needed}
\begin{block}{}
  I didn't \emph{invent} |IFunctor|s. I remembered that \emph{each}
  kind of indexed set |{i} -> *| has morphisms, |sig :->
  tau| obeying categorical laws, and I \emph{instantiated} the
  categorical notion of functor accordingly.
\end{block}
\begin{block}<2->{}
Haskell's |Functor| is \emph{just} the special case for
\emph{functors from |*| to |*|}.
\end{block}
\begin{block}<3->{}
However, |IFunctor| is a richer notion, as I may have mentioned
before. It doesn't just allow fixpoints; it's \emph{closed} under
fixpoints. But that's another story...
\end{block}
\begin{block}<4->{}
{\color{dorange}{Guess what I'm not going to invent next..?}}
\end{block}
\end{frame}

\begin{frame}{Indexed Monads}
%format IMonad = "\mathsf{\Cl{IMonad}}"
%format iskip = "\mathsf{\F{iskip}}"
%format iextend = "\mathsf{\F{iextend}}"
%format iseq = "\mathsf{\F{iseq}}"
%format ireturn = "\mathsf{\F{ireturn}}"
%format rho = "\rho"

< class IFunctor phi => IMonad (phi :: ({i} -> *) -> {i} -> *) where
<   iskip    :: sig :-> phi sig
<   iextend  :: (sig :-> phi tau) -> (phi sig :-> phi tau)

It's quite like what you're used to, but with funny names (explanation
shortly), and I've flipped `bind' (back to the way it
was when monads were `tribbles' rather than `warm fuzzy things').\\
~\\ \uncover<2->{%
Interpret |phi tau {i}| as \textbf{`|tau| is reachable from state
  |{i}|}'.}
\uncover<3->{So, |sig :-> phi tau| means
\textbf{`whenever \emph{precondition} |sig| holds,
 \emph{postcondition} |tau| is reachable'}.}
\uncover<4->{Or, as Peter
Hancock put it, \emph{`But, Conor, that's just Hoare Logic!'}.}
\uncover<5->{

< iseq :: IMonad phi => (rho :-> phi sig) -> (sig :-> phi tau) -> rho :-> phi tau
< iseq f g = iextend g . f

}
\end{frame}

\begin{frame}{Key Example: Typed Terms}

%format Ty = "\mathsf{\T{Ty}}"
%format Tm = "\mathsf{\T{Tm}}"
%format BB = "\mathsf{\C{BB}}"
%format NN = "\mathsf{\C{NN}}"
%format If = "\mathsf{\C{If}}"
%format Le = "\mathsf{\C{Le}}"
%format Add = "\mathsf{\C{Add}}"
%format Var = "\mathsf{\C{Var}}"

> data Ty = BB | NN
>
> data Tm :: ({Ty} -> *) -> {Ty} -> * where
>   Var  :: alf {t} -> Tm alf {t}
>   Le   :: Tm alf {NN} -> Tm alf {NN} -> Tm alf {BB}
>   Add  :: Tm alf {NN} -> Tm alf {NN} -> Tm alf {NN}
>   If   :: Tm alf {BB} -> Tm alf {t} -> Tm alf {t} -> Tm alf {t}

\uncover<2->{
The |IMonad| behaviour is \emph{type-respecting substitution}.

> instance IMonad Tm where
>   iskip = Var
>   iextend f  (Var x)     = f x
>   iextend f  (Le s t)    = Le (iextend f s) (iextend f t)
>   iextend f  (Add s t)   = Add (iextend f s) (iextend f t)
>   iextend f  (If b s t)  = If (iextend f b) (iextend f s) (iextend f t)

The |IFunctor| behaviour is \emph{type-respecting renaming}.

> instance IFunctor Tm where
>   imap f = iextend (Var . f)
}

\end{frame}

\begin{frame}{Free Monads (I)}
\\ ~ \\
Seen this?

< data f :* t = Ret t | Do (f (f :* t))

You can see this as a kind of `generalized syntax', where |f| describes the
\emph{constructors} but |(f :*)| chucks in \emph{variables}, too.
\uncover<2->{
The |Monad| behaviour is exactly substitution.

< instance Functor f => Monad (f :*) where
<   return = Ret
<   Ret t   >>= g = g t
<   Do fft  >>= g = Do (fmap (>>= g) fft)

Or you can think of it as the |Monad| with \emph{commands} given by |f|,
and we throw in |return|. Elements of (f :* t) are \emph{strategies} for
doing |f| commands in a quest to deliver an |t|, and |>>=| pastes stratgies
together.
}

\end{frame}

\begin{frame}{Free Monads (II)}
%format gap = "\qquad"
\\ ~ \\
Let me just rejig that |data| declaration, GADT style.

< data (:*) ::  (* -> *)  -> 
<               * -> *    where
<   Ret  :: t           -> f :* t
<   Do   :: f (f :* t)  -> f :* t

\end{frame}

\begin{frame}{Free Monads (III)}
\\ ~ \\
Let me just index that.

< data (:*) ::  (({i} -> *) -> {i} -> *)  ->
<               ({i} -> *) -> {i} -> *    where
<   Ret  ::  t           :-> f :* t
<   Do   ::  f (f :* t)  :-> f :* t

\begin{itemize}
\item<2->{|Ret| says `|t| is reachable if it's already witnessed'.}
\item<3->{|Do| says `if doing \emph{one} |f|-command makes |t| reachable,
then it's reachable already'}
\end{itemize}

< instance IFunctor f => IMonad (f :*) where
<   iskip = Ret
<   iextend g (Ret t)   = g t
<   iextend g (Do fft)  = Do (imap (iextend g) fft)

\end{frame}
\begin{frame}{Free Monads (IV)}
\\ ~ \\
Let me expand |:->| to fix the syntax errors.

< data (:*) ::  (({i} -> *) -> {i} -> *)  ->
<               ({i} -> *) -> {i} -> *    where
<   Ret  ::  t {i}           -> (f :* t) {i}
<   Do   ::  f (f :* t) {i}  -> (f :* t) {i}

What would go wrong if we expanded |type| synonyms before checking
GADT constructors?

\end{frame}

\begin{frame}{Indexed Monads, Demonic Bind}

< class IFunctor phi => IMonad (phi :: ({i} -> *) -> {i} -> *) where
<   iskip    :: sig :-> phi sig
<   iextend  :: (sig :-> phi tau) -> (phi sig :-> phi tau)

We can also define handy two infix binds. \uncover<2->{\emph{Demonic}
  bind}\only<1>{\vspace*{0.87in}}\only<2>{

< (?>=) ::  IMonad phi =>
<           phi sig {i} -> (sig :-> phi tau) -> phi tau {i}
< c ?>= f = iextend f c

}\only<3->{

< (?>=) ::  IMonad phi =>
<           forall i . phi sig {i} -> (forall j . sig {j} -> phi tau {j}) -> phi tau {i}
< c ?>= f = iextend f c

}\uncover<2->{%
models the general situation: you must be ready for \emph{any} state
satisfying |sig|.} \uncover<3->{We choose |i| but the demon (\emph{i.e.},
reality) chooses |j|.}\\
~\\
\uncover<4->{\textbf{|IMonad|s model uncertainty about the state of the
world in which computation happens, and what we can learn by interacting
with it.}}
\end{frame}

\begin{frame}{Demonic Bind, Angelic Bind}

< (?>=) ::  IMonad phi =>
<           forall i . phi sig {i} -> (forall j . sig {j} -> phi tau {j}) -> phi tau {i}
< c ?>= f = iextend f c

\uncover<2->{\emph{Angelic} bind constricts the demon with atkey.

< (=>=) ::  IMonad phi => phi (a :- {j}) {i} -> (a -> phi tau {j}) -> phi tau {i}
< c =>= f = c ?>= \ (V a) -> f a

}\uncover<3->{\vspace*{-0.3in}

< ireturn :: IMonad phi => a -> phi (a :- {i}) {i}
< ireturn a = iskip (V a)

}\uncover<4->{%
You can rebind |return| to |ireturn| and |>>=| to |=>=|.}
\uncover<5->{Put |tau = b :- {k}|

< (=>=) ::  IMonad phi  => phi  (a :- {j}) {i}
<                       ->      (a -> phi (b :- {k})  {j}) -> phi (b :- {k})  {i}

}\uncover<6->{%
\emph{cf} Wadler, Uustalu, Kiselyov, Brady,...
}\uncover<7->{%
and Bob of that ilk.
}
\end{frame}

\begin{frame}{Angelic Applicatives}
%format IApplicative = "\mathsf{\Cl{IApplicative}}"
%format Applicative = "\mathsf{\Cl{Applicative}}"
%format pure = "\mathsf{\F{pure}}"
%format <*> = "\mathbin{\F{\oast}}"
\\ ~\\
While I'm about it, let me define

> class IFunctor phi => IApplicative (phi :: ({i} -> *) -> {i} -> *) where
>   pure   ::  x -> phi (x :- {i}) {i}
>   (<*>)  ::  phi ((s -> t) :- {j}) {i} ->
>              phi (s :- {k}) {j} -> phi (t :- {k}) {i}

\uncover<2->{This says |phi| allows us to build applications by (angelic)
computation. |pure| computations preserve the state; |<*>| computes the
function whilst evolving from |{i}| to |{j}| and its argument whilst evolving
from |{j}| to |{k}|.}
\\ ~\\
\uncover<3->{We're still lifting `ordinary programming' to an effectful world,}
\uncover<4->{but now we're playing dominos, too.}\\ ~\\
\uncover<5->{Every |IMonad| is |IApplicative|, just
as when we work over |*|.}

\end{frame}

\begin{frame}{Digressing further, let's peel those bananas...}
%format a1 = "a_1"
%format an = "a_n"
%format <* = "\mathbin{\F{<\!\!\!\ast}}"
%format const = "\mathsf{\F{const}}"

< fileContents ::  FilePath ->
<                  (FH :* (Maybe String :- {Closed})) {Closed}
< fileContents p = fOpen p ?>= \ s -> case s of
<   {Closed}  -> (| Nothing |)
<   {Open}    -> (| Just readOpenFile (-fClose-) |)

\SHE{} turns applications

<  (|f a1 ... an|)

in \emph{idiom} brackets into

<  pure f <*> a1 <*> ... <*> an

like in the paper by Ross and me, but round.
\uncover<2->{
Meanwhile, \emph{noise brackets}, |(- ... -)|, tack in effects but
ignore their values, using |<*|:

< thing <* noise = (| const thing noise |)

}
\uncover<3->{%
Above, we get |Just| the |String| from the file, \emph{and} we
|fClose| the file.
}
\end{frame}

\begin{frame}{Idiom Brackets de luxe}

< readOpenFile :: (FH :* (String :- {Open})) {Open}
< readOpenFile = fGetC =>= \ x -> case x of
<   Nothing  ->  (| nil |)
<   Just c   ->  (| ~c : readOpenFile |)

\uncover<2->{%
\SHE{} notices when the function in an application is
infix.
}\\ ~ \\
\uncover<3->{%
\SHE{} lets you mark pure \emph{arguments} with |~|, so |~c| means
|pure c|.
}\\ ~ \\
\uncover<4->{%
I'm using idiom brackets with |IApplicative| here, but they also work for
|Applicative|.
}\\ ~ \\
\uncover<5->{%
Syntax remains negotiable: I'm open to suggestions.
}\\ ~ \\
\uncover<6->{\emph{Ha ha}: |(-3-)|.}

\end{frame}


\begin{frame}{Where were we before we bananaed off?}
%format ups = "\upsilon"
%format psi = "\psi"
\\ ~\\
We'd seen how to get a free monad from a functor describing commands.
Here's a functor which describes commands via \emph{Hoare Logic}.

> data (sig :>>: tau) ups {i} =  sig {i}           -- precondition holds now
>                                :& (tau :-> ups)  -- postcondition delivers goal

We can reach |ups| by doing a |(sig :>>: tau)| command if |sig| holds now,
and we can get |ups| from |tau|.
\uncover<2->{

> data (IFunctor phi, IFunctor psi) => (phi :+: psi) tau {i}
>   =  InL  (phi tau {i})
>   |  InR  (psi tau {i})

|IFunctor| is closed under \emph{choice}, so you can offer a choice of commands.
}
\end{frame}

\begin{frame}{That File System}
\\ ~ \\

< type FH -- |:: ({State} -> *) -> {State} -> *|
<   =    FilePath  :- {Closed}  :>>: (:: State)            -- fOpen
<   :+:  Unit      :- {Open}    :>>: Maybe Char :- {Open}  -- fGetC
<   :+:  Unit      :- {Open}    :>>: Unit :- {Closed}      -- fClose

It's a choice of commands, specified in Hoare Logic. We get the corresponding
|IMonad|, |(FH :*)| at no extra charge.
\\ ~ \\
\uncover<2->{But what's that |(:: State)|?}
\\ ~ \\
\uncover<3->{We can't predict the state after |fOpen|. We rather need to
\emph{check} it at run time.}

\end{frame}

\begin{frame}{Dependent Types to the Rescue}
%format pi = "\mathkw{pi}"
\\ ~ \\
When you write...

< data State :: * where
<   Open    :: State
<   Closed  :: State
<   deriving SheSingleton  -- ...this...

\uncover<2->{... \SHE{} constructs this (with uglier underwater names):

< (:: State) :: {State} -> *
< {Open}    :: (:: State) {Open}
< {Closed}  :: (:: State) {Closed}

}%
\uncover<3->{%
The point: if you do |case| analysis on a |(:: State) {i}|, you find
out what |i| is.
}\\ ~ \\
\uncover<4->{%
\SHE{} takes |pi (x :: s) . t| to mean |forall x . (:: s) {x} -> t|
}
\end{frame}

\begin{frame}{Putting it all together}

< fileContents ::  FilePath ->
<                  (FH :* (Maybe String :- {Closed})) {Closed}
< fileContents p = fOpen p ?>= \ s -> case s of
<   {Closed}  -> (| Nothing |)
<   {Open}    -> (| Just readOpenFile (-fClose-) |)

We \emph{must} check if the file is open before reading it.
We \emph{must} close the file at the end.

< readOpenFile :: (FH :* (String :- {Open})) {Open}
< readOpenFile = fGetC =>= \ x -> case x of
<   Nothing  ->  (| nil |)
<   Just c   ->  (| ~c : readOpenFile |)

\uncover<2->{We've captured a policy for safe interaction with a dangerous
world.}

\end{frame}

\begin{frame}{Congratulations, Haskell!}
\includegraphics[height=7cm]{winging12.jpg}\\
\uncover<2->{You're the world's first mainstream dependently typed programming
language!}
\end{frame}

\begin{frame}{The Scottish Society for the Prevention of Cruelty to
    Simons}

\begin{block}{}
confirms that no Simons were harmed in the making of this motion picture.
\end{block}

\end{frame}

\end{document}
