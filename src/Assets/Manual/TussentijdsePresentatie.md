
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

$$STFL.language![45..46],[51..59]!indent


















