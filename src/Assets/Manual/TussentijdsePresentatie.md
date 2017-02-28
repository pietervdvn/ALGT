
 Building Gradualized typesystems
====================================


Wat is een gradueel typesysteem?
--------------------------------

Sommige typesystemen zijn krachtiger dan andere:

- Python is volledig dynamisch
- Java heeft statische typechecking
- Haskell heeft een nog krachtiger typesysteem

Een gradueel typesysteem introduceert _onzekerheid_ 

Delen van de code zijn statisch getypeerd, andere zijn afgezwakt

De programmeur kiest zelf de mate waarin

Waarom is een gradueel typesysteem nuttig?
------------------------------------------

Veel software begint met een _PoC_ in een dynamische taal (e.g. Python)\
\

Deze POC wordt vaak uitgebreid en uitgebreid totdat het moeilijk wordt om dit onder controle te houden...


Waarom is een gradueel typesysteem nuttig?
------------------------------------------

Veel software begint met een _PoC_ in een dynamische taal (e.g. Python)\
\

Deze POC wordt vaak uitgebreid en uitgebreid totdat het moeilijk wordt om dit onder controle te houden zonder typesysteem...\
\
... maar ook duur om te herschrijven

Waarom is een gradueel typesysteem nuttig?
------------------------------------------

Veel software begint met een _PoC_ in een dynamische taal (e.g. Python)\
\

Deze POC wordt vaak uitgebreid en uitgebreid totdat het moeilijk wordt om dit onder controle te houden zonder typesysteem...\
\
... maar ook duur om te herschrijven


Waarom is een gradueel typesysteem nuttig?
------------------------------------------

Met een gradueel typesysteem begin je met een dynmische PoC\
\

Wanneer de software groeit, voegt de programmeur type-annotaties toe en krijg je meer statische garanties\
\

De programmeur kiest zelf hoeveel typering nodig is


Waarom zijn er niet meer graduele typesystemen?
-----------------------------------------------

Een typesysteem gradualizeren is moeilijk en handmatig werk

Mijn tool __automatiseert__ het gradualizeren van typesystemen voor __arbitraire talen__


 Gradualizatie
===============


Gradualizatie
-------------

 - Invoeren van de programmeertaal
 - Bouwen van een dynamische runtime
 - Bouwen van een gradueel typesysteem

Snel en efficiënt


Definitie van een programmeertaal
=================================

Simply Typed Functional Language (_STFL_)
-----------------------------------------

We bouwen samen een simpele functionele programmeertaal


$$STFL.language![0..5]!indent


STFL: Syntax
------------

Vereenvoudigde BNF-notatie


$$STFL.language![5..11]!indent


STFL: Syntax
------------


$$STFL.language![12..26]!indent


STFL: Functions
---------------

Pattern matching clauses

$$STFL.language![30..42]!indent


STFL: Relaties
--------------

We creëeren relaties om _natural deduction_ op toe te passen, zoals gangbaar in de academische wereld



Dit kan met unicode-karakters (maar niet in deze slides...)

STFL: Relaties, declaratie
--------------

$$STFLForSlides.language![45..46],[51..59]!indent!safe

STFL: Rules
--------------

$$STFLForSlides.language![62..71]!indent!safe

STFL: Rules
--------------

$$STFLForSlides.language![74..76]!indent!safe

STFL: Rules
--------------

$$STFLForSlides.language![78..81]!indent!safe

STFL: Rules
--------------

$$STFLForSlides.language![84..86]!indent!safe


STFL: Rules
-----------

$$STFLForSlides.language![158..161]!indent!safe


STFL: Rules
-----------

_En nog een hoop andere regels_


STFL: properties
----------------

$$STFLForSlides.language![187..197]!indent!safe


 Opstellen van bewijsbomen
===========================


Bewijsboom voor evaluatie
-------------------------

	------------------------------------ [EvalIfTrue]
	If True Then False Else True -> False

Bewijsboom voor evaluatie
-------------------------

	7 : number    8 : number
	------------------------ [EvalPlus]
	7 + 8 -> 15
	---------------------------------- [EvalCtx]
	5 + ( 6 + ( 7 + 8 ) ) -> 5 + ( 6 + ( 15 ) )


Bewijsboom voor typering
------------------------

	True : bool       False : bool        
	---------------   -----------------    
	{} |- True, Bool  {} |- False, Bool    Bool == Bool
	-----------------------------------------------------
	{} |- If True Then False Else True, Bool
	-----------------------------------------------------
	If True Then False Else True :: Bool



Bewijsboom voor typering
------------------------


                         6 : number      7 : number
                         -----------    ------------
	5 : number       {} |- 6, Int   {} |- 7, Int
	------------    ----------------------------
	{} |- 5, Int     {} |- 6 + 7, Int
	--------------------------------------------
	{} |- 5 + 6 + 7, Int
	--------------------------------------------
	5 + 6 + 7 :: Int




Bouwen van een dynamische runtime
=================================

AGT-paper
---------

Gebaseerd op de paper _Abstracting Gradual Typing_ (Ronald Garcia, Alison M.Clark, Éric Tanter)


Bewijs bijhouden
----------------

We houden bewijs bij dat een typering _mogelijk_ is

We genereren nieuwe syntactische vormen met bewijs:

	proofTyping	::= "<" type "," type ">"

En 'plakken' dit aan elke expressie:

	provenExpr	::= proofTyping expr


Bewijs bijhouden
------------------

Zo kunnen we een mogelijk bewijs bijhouden:

	(\\x . ? : x + 1) True

wordt:

	 <Bool, Bool> (\\x . ? : x + 1) True

Want `Bool` is consistent met `?`

Bij de volgende stap kan er geen bewijs meer gevonden worden, dus hebben we een type error

	< ? > (True :: Int) + 1







