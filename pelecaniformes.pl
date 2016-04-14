%CPSC 449 Prolog Assignment
%By: Arthur Iwaniszyn, Kyle Perry and Bernie Mayer.

%order(?A).
%This predicate will succeed if A is a order.
order(pelecaniformes).

%family(?A).
%This predicate will succeed if A is a family
family(pelecanidae).
family(ardeidae).
family(threskiornithdae).

%genus(?A).
%This predicate will succeed if A is a genus
genus(pelecanus).
genus(botaurus).
genus(ixobrychus).
genus(ardea).
genus(egretta).
genus(bubulcus).
genus(butorides).
genus(nycticorax).
genus(nyctanassa).
genus(eudocimus).
genus(plegadis).
genus(platalea).


%species(?A).
%This predicate will succeed if A is a species
species(occidentalis).
species(erythrorhynchos).
species(lentiginosus).
species(exilis).
species(herodias).
species(alba).
species(thula).
species(caerulea).
species(tricolor).
species(rufescens).
species(ibis).
species(virescens).
species(nycticorax).
species(violacea).
species(albus).
species(falcinellus).
species(chihi).
species(ajaja).

%hasParent(?A, ?B).
%This predicate will succeed if B is a direct parent of A.
hasParent(ajaja, platalea).
hasParent(chihi, plegadis).
hasParent(falcinellus, plegadis).
hasParent(albus, eudocimus).
hasParent(violacea, nyctanassa).
hasParent(nycticorax, nycticorax).
hasParent(virescens, butorides).
hasParent(ibis, bubulcus).
hasParent(rufescens, egretta).
hasParent(tricolor, egretta).
hasParent(caerulea, egretta).
hasParent(thula, egretta).
hasParent(alba, ardea).
hasParent(herodias, ardea).
hasParent(exilis, ixobrychus).
hasParent(lentiginosus, botaurus).
hasParent(occidentalis, pelecanus).
hasParent(erythrorhynchos, pelecanus).
hasParent(pelecanus, pelecanidae).
hasParent(botaurus, ardeidae).
hasParent(ixobrychus, ardeidae).
hasParent(ardea, ardeidae).
hasParent(egretta, ardeidae).
hasParent(bubulcus, ardeidae).
hasParent(butorides, ardeidae).
hasParent(nycticorax, ardeidae).
hasParent(nyctanassa, ardeidae).
hasParent(eudocimus, threskiornithdae).
hasParent(plegadis, threskiornithdae).
hasParent(platalea, threskiornithdae).
hasParent(threskiornithdae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(pelecanidae, pelecaniformes).

%hasCommonName(?N, ?C).
%This predicate will succeed if The taxonomical name N has a common name C. If N is a species name, it must be a compound name.
hasCommonName(Y, americanWhitePelican):- (atom_concat('pelecanus_', 'erythrorhynchos', Y)).
hasCommonName(Y, brownPelican):- (atom_concat('pelecanus_', 'occidentalis', Y)).
hasCommonName(Y, americanBittern):- (atom_concat('botaurus_', 'lentiginosus', Y)).
hasCommonName(Y, leastBittern):- (atom_concat('ixobrychus_', 'exilis', Y)).
hasCommonName(Y, greatBlueHeron):- (atom_concat('ardea_', 'herodias', Y)).
hasCommonName(Y, greatEgret):- (atom_concat('ardea_', 'alba', Y)).
hasCommonName(Y, snowyEgret):- (atom_concat('egretta_', 'thula', Y)).
hasCommonName(Y, littleBlueHeron):- (atom_concat('egretta_', 'caerulea', Y)).
hasCommonName(Y, tricoloredHeron):- (atom_concat('egretta_', 'tricolor', Y)).
hasCommonName(Y, reddishEgret):- (atom_concat('egretta_', 'rufescens', Y)).
hasCommonName(Y, cattleEgret):- (atom_concat('bubulcus_', 'ibis', Y)).
hasCommonName(Y, greenHeron):- (atom_concat('butorides_', 'virescens', Y)).
hasCommonName(Y, nycticorax):- (atom_concat('nycticorax_', 'nycticorax', Y)).
hasCommonName(Y, yellowCrownedNightHeron):- (atom_concat('nyctanassa_', 'violacea', Y)).
hasCommonName(Y, whiteIbis):- (atom_concat('eudocimus_', 'albus', Y)).
hasCommonName(Y, glossyIbis):- (atom_concat('plegadis_', 'falcinellus', Y)).
hasCommonName(Y, whiteFacedIbis):- (atom_concat('plegadis_', 'chihi', Y)).
hasCommonName(Y, roseateSpoonbill) :- (atom_concat('platalea_', 'ajaja', Y)). 
hasCommonName(pelecanus, pelican).
hasCommonName(botaurus, bittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ardea, heron).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(bubulcus, egret).
hasCommonName(butorides, heron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(eudocimus, ibis).
hasCommonName(plegadis, ibis).
hasCommonName(platalea, spoonbill).

%hasCommonName(?G, ?S, ?C).
%this predicate succeeds if a species described by a genus G and raw species name S, has a common name C.
hasCommonName(pelecanus, erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus, occidentalis, brownPelican).
hasCommonName(botaurus, lentiginosus, americanBittern).
hasCommonName(ixobrychus, exilis, leastBittern).
hasCommonName(ardea, herodias, greatBlueHeron).
hasCommonName(ardea, alba, greatEgret).
hasCommonName(egretta, thula, snowyEgret).
hasCommonName(egretta, caerulea, littleBlueHeron).
hasCommonName(egretta, tricolor, tricoloredHeron).
hasCommonName(egretta, rufescens, reddishEgret).
hasCommonName(bubulcus, ibis, cattleEgret).
hasCommonName(butorides, virescens, greenHeron).
hasCommonName(nycticorax, nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus, albus, whiteIbis).
hasCommonName(plegadis, falcinellus, glossyIbis).
hasCommonName(plegadis, chihi, whiteFacedIbis).
hasCommonName(platalea, ajaja, roseateSpoonbill).

%hasSciName(?C, ?N).
%N is a compound taxonomical name for some species that has a common name C, or N is an order, family, or genus that has a common name C.
hasSciName(C,N) :-commonAndGenus(N,C);provideFull(_,_,_,N,C).

% commonAndGenus is a predicate that is used in hasSciName, it returns true if a the common name has the genus.  
commonAndGenus(pelecanus, pelican).
commonAndGenus(botaurus, bittern).
commonAndGenus(ixobrychus,bittern).
commonAndGenus(ardea,heron).
commonAndGenus(egretta,heron).
commonAndGenus(egretta,egret).
commonAndGenus(bublucus,egret).
commonAndGenus(butorides,heron).
commonAndGenus(nycticorax, nightHeron).
commonAndGenus(nyctanassa, nightHeron).
commonAndGenus(eudocimus, ibis).
commonAndGenus(plegadis, ibis).
commonAndGenus(platalea,spoonbill).

%hasCompoundName(?G, ?S, ?N).
%This predicate succeeds if N is a compound name for the genus G and species S.
hasCompoundName(pelecanus, erythrorhynchos, Y):- (atom_concat('pelecanus_', 'erythrorhynchos', Y)).
hasCompoundName(pelecanus, occidentalis, Y):- (atom_concat('pelecanus_', 'occidentalis', Y)).
hasCompoundName(botaurus, lentiginosus, Y):- (atom_concat('botaurus_', 'lentiginosus', Y)).
hasCompoundName(ixobrychus, exilis, Y):- (atom_concat('ixobrychus_', 'exilis', Y)).
hasCompoundName(ardea, herodias, Y):- (atom_concat('ardea_', 'herodias', Y)).
hasCompoundName(ardea, alba, Y):- (atom_concat('ardea_', 'alba', Y)).
hasCompoundName(egretta, thula, Y):- (atom_concat('egretta_', 'thula', Y)).
hasCompoundName(egretta, caerulea, Y):- (atom_concat('egretta_', 'caerulea', Y)).
hasCompoundName(egretta, tricolor, Y):- (atom_concat('egretta_', 'tricolor', Y)).
hasCompoundName(egretta, rufescens, Y):- (atom_concat('egretta_', 'rufescens', Y)).
hasCompoundName(bubulcus, ibis, Y):- (atom_concat('bubulcus_', 'ibis', Y)).
hasCompoundName(butorides, virescens, Y):- (atom_concat('butorides_', 'virescens', Y)).
hasCompoundName(nycticorax, nycticorax, Y):- (atom_concat('nycticorax_', 'nycticorax', Y)).
hasCompoundName(nyctanassa, violacea, Y):- (atom_concat('nyctanassa_', 'violacea', Y)).
hasCompoundName(eudocimus, albus, Y):- (atom_concat('eudocimus_', 'albus', Y)).
hasCompoundName(plegadis, falcinellus, Y) :- (atom_concat('plegadis_', 'falcinellus', Y)).
hasCompoundName(plegadis, chihi, Y) :- (atom_concat('plegadis_', 'chihi', Y)).
hasCompoundName(platalea, ajaja, Y) :- (atom_concat('platalea_', 'ajaja', Y)).


%isaStrict(?A, ?B).
%isaStrict will succeed if A is the same as B or if A is an ancestor of B (The parent of A, the parent of the parent of A etc)

isaStrict(A, B) :- convertToSpeciesName(A,X), convertToSpeciesName(B,Y), (\+ species(A); genus(A)), (\+species(B); genus(B)),  isaStrictActual(X,Y).
isaStrict(A, B) :- convertToSpeciesName(A,X), isaStrictActual(X,B), (\+ species(A); genus(A)).
isaStrict(A, B) :- convertToSpeciesName(B,Y), isaStrictActual(A,Y), (\+ species(B); genus(B)).
isaStrict(A, B) :- isaStrictActual(A,B), (\+ species(A); genus(A)), (\+species(B); genus(B)).


%isaStrictActual is a helper predicate for isaStrict
%isaStrictActual is helpful with dealing with compound scpecies names
%it is the fucnction that will actually see if A is the ancestor of B.

isaStrictActual(A, B) :- A == B,( (species(A), species(B)); (genus(A), genus(B)); (family(A), family(B)); (order(A), order(B))).
isaStrictActual(A, B) :- hasParent(A, C), hasParent(C, D), hasParent(D, B).
isaStrictActual(A, B) :- hasParent(A, C), hasParent(C, B).
isaStrictActual(A, B) :- hasParent(A,B).

%converts A to its species name B if A has a species name, is a helper predicate

convertToSpeciesName(A,B) :- hasCompoundName( _, B, A).
 
% giveRaw is a function that will give us the raw name of a species given a compound name.

giveRaw(Y,erythrorhynchos):- (atom_concat('pelecanus_', 'erythrorhynchos', Y)). 
giveRaw(Y,occidentalis):- (atom_concat('pelecanus_', 'occidentalis', Y)). 
giveRaw(Y,lentiginosus):- (atom_concat('botaurus_', 'lentiginosus', Y)). 
giveRaw(Y,exilis):- (atom_concat('ixobrychus_', 'exilis', Y)).
giveRaw(Y,herodias):- (atom_concat('ardea_', 'herodias', Y)).
giveRaw(Y,alba):- (atom_concat('ardea_', 'alba', Y)). 
giveRaw(Y,thula):- (atom_concat('egretta_', 'thula', Y)).
giveRaw(Y,caerulea):- (atom_concat('egretta_', 'caerulea', Y)). 
giveRaw(Y,tricolor):- (atom_concat('egretta_', 'tricolor', Y)). 
giveRaw(Y,rufescens):- (atom_concat('egretta_', 'rufescens', Y)).
giveRaw(Y,ibis):- (atom_concat('bubulcus_', 'ibis', Y)). 
giveRaw(Y,virescens):- (atom_concat('butorides_', 'virescens', Y)).
giveRaw(Y,nycticorax):- (atom_concat('nycticorax_', 'nycticorax', Y)).
giveRaw(Y,violacea):- (atom_concat('nyctanassa_', 'violacea', Y)).
giveRaw(Y,albus):- (atom_concat('eudocimus_', 'albus', Y)). 
giveRaw(Y,falcinellus):- (atom_concat('plegadis_', 'falcinellus', Y)).
giveRaw(Y,chihi):- (atom_concat('plegadis_', 'chihi', Y)). 
giveRaw(Y,ajaja):- (atom_concat('platalea_', 'ajaja', Y)). 


% provideFull is a function that will give us the order, family, genus, species and common name of the species.
  
provideFull(pelecaniformes, pelecanidae, pelecanus, Y, americanWhitePelican):- (atom_concat('pelecanus_', 'erythrorhynchos', Y)). 
provideFull(pelecaniformes, pelecanidae, pelecanus, Y, brownPelican):- (atom_concat('pelecanus_', 'occidentalis', Y)). 
provideFull(pelecaniformes, ardeidae, botaurus, Y, americanBittern):- (atom_concat('botaurus_', 'lentiginosus', Y)).  
provideFull(pelecaniformes, ardeidae, ixobrychus, Y, leastBittern):- (atom_concat('ixobrychus_', 'exilis', Y)).
provideFull(pelecaniformes, ardeidae, ardea, Y, greatBlueHeron):- (atom_concat('ardea_', 'herodias', Y)).
provideFull(pelecaniformes, ardeidae, ardea,Y,greatEgret):- (atom_concat('ardea_', 'alba', Y)).  
provideFull(pelecaniformes, ardeidae, egretta, Y, snowyEgret):- (atom_concat('egretta_', 'thula', Y)). 
provideFull(pelecaniformes, ardeidae,egretta, Y, littleBlueHeron):- (atom_concat('egretta_', 'caerulea', Y)). 
provideFull(pelecaniformes, ardeidae,egretta, Y, tricoloredHeron):- (atom_concat('egretta_', 'tricolor', Y)). 
provideFull(pelecaniformes, ardeidae,egretta, Y, reddishEgret):- (atom_concat('egretta_', 'rufescens', Y)). 
provideFull(pelecaniformes, ardeidae,bubulcus, Y, cattleEgret):- (atom_concat('bubulcus_', 'ibis', Y)). 
provideFull(pelecaniformes, ardeidae,butorides, Y, greenHeron):- (atom_concat('butorides_', 'virescens', Y)). 
provideFull(pelecaniformes, ardeidae,nycticorax, Y, blackCrownedNightHeron):- (atom_concat('nycticorax_', 'nycticorax', Y)). 
provideFull(pelecaniformes, ardeidae,nyctanassa, Y, yellowCrownedNightHeron):- (atom_concat('nyctanassa_', 'violacea', Y)). 
provideFull(pelecaniformes, threskiornithdae, eudocimus, Y, whiteIbis):- (atom_concat('eudocimus_', 'albus', Y)).  
provideFull(pelecaniformes, threskiornithdae, plegadis, Y, glossyIbis):- (atom_concat('plegadis_', 'falcinellus', Y)).
provideFull(pelecaniformes, threskiornithdae, plegadis, Y, whiteFacedIbis):- (atom_concat('plegadis_', 'chihi', Y)).  
provideFull(pelecaniformes, threskiornithdae, platalea, Y, roseateSpoonbill) :- (atom_concat('platalea_', 'ajaja', Y)). 



% hasParent2(A,B) return true if A is a compound name and if B is its parent. 
 
hasParent2(A,B) :- giveRaw(A,_), provideFull(_,_,B,A,_). 
hasParent2(A,B) :- family(A), provideFull(B,A,_,_,_). 
hasParent2(A,B) :- genus(A), provideFull(_,B,A,_,_). 

/*
 *	rangesTo(?X,?Y)
 *
 *	Gives us the ranges of compound species if X is a variable
 *	Otherwise, gets the range of a given Genus, Order, Family, or Compound Species name.
 */
rangesTo(X,Y):- var(X) -> hasCompoundName(_,_,X), rangeOf(X,Y).
rangesTo(X,Y):- atom(X) -> rangeOf(X,Y).

/*
 *	Database for of ranges for pelicans, used in rangesTo
 */
rangeOf(pelecaniformes, canada).
rangeOf(pelecaniformes, alberta).
rangeOf(pelecanidae, canada).
rangeOf(pelecanidae, alberta).
rangeOf(pelecanus, canada).
rangeOf(pelecanus, alberta).
rangeOf(ardeidae, canada).
rangeOf(ardeidae, alberta).
rangeOf(botaurus, canada).
rangeOf(botaurus, alberta).
rangeOf(bubulcus,canada).
rangeOf(ardea, canada).
rangeOf(ardea, alberta).
rangeOf(butorides, canada).
rangeOf(nycticorax, canada).
rangeOf(nycticorax, alberta).
rangeOf(pelecanus_erythrorhynchos, canada).
rangeOf(pelecanus_erythrorhynchos, alberta).
rangeOf(botaurus_lentiginosus, canada).
rangeOf(botaurus_lentiginosus, alberta).
rangeOf(bubulcus_ibis, canada).
rangeOf(ardea_herodias, canada).
rangeOf(ardea_herodias, alberta).
rangeOf(ardea_alba, canada).
rangeOf(butorides_virescens, canada).
rangeOf(nycticorax_nycticorax, canada).
rangeOf(nycticorax_nycticorax, alberta).


/*
 *	countSpecies(?B,?N)
 *
 *	Counts how many species there are for B. N is the number that belong to it
 *	- If B is a compound name, then N is 1.
 *	- If B is an order, family, or genus name, then N is the number of species belonging to them
 *	- If B is anything else, N is 0.
*/
countSpecies(B, N) :- hasCompoundName(_,_,B) -> N is 1.
countSpecies(B, N) :- genus(B) -> findall(X, hasParent(X,B), L) -> length(L, Y), N is Y.
countSpecies(B, N) :- (family(B); order(B)) -> findall(X, hasParent(X,B), L) -> maplist(countSpecies, L, L1) -> listsum(L1, N).
countSpecies(B, 0) :- \+((family(B);order(B);genus(B);hasCompoundName(_,_,B))).

%Sums a list of numbers together. X is the sum of the list

listsum([X], X).                  
listsum([H|L], X):- listsum(L, Y), X is (H + Y).


/*
 *	synonym(?A,?B)
 *
 *	Checks if two names are a synonym of each other
 *	Two names are synonyms if:
 *	- A is a common name of B or vice versa
 *	- A and B are common names of another name X, but A and B are not the same.
*/						 
synonym(A,B)	:-	(hasCommonName(A,B);hasCommonName(B,A);(hasCommonName(X,A),hasCommonName(X,B),A\==B)).





%isa(?A, ?B).
%This predicate is similar to isaStrict, however will work with Common Names
%However you can't have a variable be mapped to a common name in this predicate

isa(A,B) :- \+ var(A), hasCommonName(C,A), \+ var(B),hasCommonName(D,B), isaStrict(C,D).
isa(A,B) :- \+ var(A), hasCommonName(C,A), isNonCommonName(B), isaStrict(C,B).
isa(A,B) :- \+ var(B), hasCommonName(D,B), isNonCommonName(A), isaStrict(A,D).
isa(A,B) :- isNonCommonName(A), B=A.
isa(A,B) :- isNonCommonName(B), A=B.
isa(A,B) :- isConverted(A,X), isConverted(B,Y), isaStrict(X,Y).
isa(A,B) :- isConverted(A,X), isaStrict(X,B).
isa(A,B) :- isConverted(B,Y), isaStrict(A,Y).
isa(A,B) :- isaStrict(A,B).



%Used to check if A has a Compound Name, helper to isa

isConverted(A,B) :- hasCompoundName(_, B ,A).


%This predicate is another helper of isa, checks if A is a compound name,
%is a order, family or a genus

isNonCommonName(A) :- hasCompoundName(_, _, A).
isNonCommonName(A) :- order(A).
isNonCommonName(A) :- family(A).
isNonCommonName(A) :- genus(A).





%//////////////////////////////////////////////////////////////////////////////////////////

%habitat(?A, ?B).
%this predicate succeeds if A prefers a habitat of B, where B is lakePond, ocean, or marsh. A may be a compound species name, genus, family, or order, but may not be a raw species name.

habitat(X,Y):- var(X) -> hasCompoundName(_,_,X), habitatOf(X,Y).
habitat(X,Y):- atom(X) -> habitatOf(X,Y).

%this is where we store the habitats of different pelicans, we use this in our habitat predicate.

habitatOf(pelecanus_erythrorhynchos, lakePond).
habitatOf(pelecanus_occidentalis, ocean).
habitatOf(botaurus_lentiginosus, marsh).
habitatOf(ixobrychus_exilis, marsh).
habitatOf(ardea_herodias, marsh).
habitatOf(ardea_alba, marsh).
habitatOf(egretta_thula, marsh).
habitatOf(egretta_caerulea, marsh).
habitatOf(egretta_tricolor, marsh).
habitatOf(egretta_rufescens, marsh).
habitatOf(bubulcus_ibis, marsh).
habitatOf(butorides_virescens, marsh).
habitatOf(nycticorax_nycticorax, marsh).
habitatOf(nyctanassa_violacea, marsh).
habitatOf(eudocimus_albus, marsh).
habitatOf(plegadis_falcinellus, marsh).
habitatOf(plegadis_chihi, marsh).
habitatOf(platalea_ajaja, marsh).
habitatOf(platalea, marsh).
habitatOf(plegadis, marsh).
habitatOf(eudocimus, marsh).
habitatOf(nyctanassa, marsh).
habitatOf(nycticorax, marsh).
habitatOf(bubulcus, marsh).
habitatOf(butorides, marsh).
habitatOf(egretta, marsh).
habitatOf(ardea, marsh).
habitatOf(ixobrychus, marsh).
habitatOf(pelecanus, ocean).
habitatOf(pelecanus, lakePond).
habitatOf(pelecanidae, ocean).
habitatOf(pelecanidae, lakePond).
habitatOf(ardeidae, marsh).
habitatOf(threskiornithdae, marsh).
habitatOf(pelecaniformes, ocean).
habitatOf(pelecaniformes, lakePond).
habitatOf(pelecaniformes, marsh).
%//////////////////////////////////////////////////////////////////////////////////////////

%//////////////////////////////////////////////////////////////////////////////////////////

%food(?A, ?B).
%This predicate returns true if A prefers to eat B. A may be a compound species name, genus, family or order, but cannot be a raw species name.

food(X,Y):- var(X) -> hasCompoundName(_,_,X), foodOf(X,Y).
food(X,Y):- atom(X) -> foodOf(X,Y).

%This is where we store the foods that different pelicans eat. We use this in our food predicate.  

foodOf(pelecanus_erythrorhynchos, fish).
foodOf(pelecanus_occidentalis, fish).
foodOf(botaurus_lentiginosus, fish).
foodOf(ixobrychus_exilis, fish).
foodOf(ardea_herodias, fish).
foodOf(ardea_alba, fish).
foodOf(egretta_thula, fish).
foodOf(egretta_caerulea, fish).
foodOf(egretta_tricolor, fish).
foodOf(egretta_rufescens, fish).
foodOf(bubulcus_ibis, insects).
foodOf(butorides_virescens, fish).
foodOf(nycticorax_nycticorax, fish).
foodOf(nyctanassa_violacea, insects).
foodOf(eudocimus_albus, insects).
foodOf(plegadis_falcinellus, insects).
foodOf(plegadis_chihi, insects).
foodOf(platalea_ajaja, fish).
foodOf(platalea, fish).
foodOf(plegadis, insects).
foodOf(eudocimus, insects).
foodOf(nyctanassa, insects).
foodOf(nycticorax, fish).
foodOf(butorides, fish).
foodOf(bubulcus, insects).
foodOf(egretta, fish).
foodOf(ardea, fish).
foodOf(ixobrychus, fish).
foodOf(pelecanus, fish).
foodOf(pelecanidae, fish).
foodOf(ardeidae, fish).
foodOf(ardeidae, insects).
foodOf(threskiornithdae, fish).
foodOf(threskiornithdae, insects).
foodOf(pelecaniformes, insects).
foodOf(pelecaniformes, fish).
%//////////////////////////////////////////////////////////////////////////////////////////

%//////////////////////////////////////////////////////////////////////////////////////////
%nesting(?A, ?B).
%This predicate returns true if A prefers to nest in B. A may be a compound species name, genus, family, or order, but cannot be a raw species name.

nesting(X,Y):- var(X) -> hasCompoundName(_,_,X), nestingOf(X,Y).
nesting(X,Y):- atom(X) -> nestingOf(X,Y).

%This is where we store the nesting information of different pelicans, we use this in our nesting predicate.

nestingOf(pelecanus_erythrorhynchos, ground).
nestingOf(pelecanus_occidentalis, tree).
nestingOf(botaurus_lentiginosus, ground).
nestingOf(ixobrychus_exilis, ground).
nestingOf(ardea_herodias, tree).
nestingOf(ardea_alba, tree).
nestingOf(egretta_thula, tree).
nestingOf(egretta_caerulea, tree).
nestingOf(egretta_tricolor, tree).
nestingOf(egretta_rufescens, tree).
nestingOf(bubulcus_ibis, tree).
nestingOf(butorides_virescens, tree).
nestingOf(nycticorax_nycticorax, tree).
nestingOf(nyctanassa_violacea, tree).
nestingOf(eudocimus_albus, tree).
nestingOf(plegadis_falcinellus, ground).
nestingOf(plegadis_chihi, ground).
nestingOf(platalea_ajaja, tree).
nestingOf(pelecanus, ground).
nestingOf(pelecanus, tree).
nestingOf(botaurus, ground).
nestingOf(ixobrychus, ground).
nestingOf(ardea, tree).
nestingOf(egretta, tree).
nestingOf(bubulcus, tree).
nestingOf(butorides, tree).
nestingOf(nycticorax, tree).
nestingOf(nyctanassa, tree).
nestingOf(eudocimus, tree).
nestingOf(plegadis, ground).
nestingOf(platalea, tree).
nestingOf(pelecanidae, ground).
nestingOf(pelecanidae, tree).
nestingOf(ardeidae, ground).
nestingOf(ardeidae, tree).
nestingOf(threskiornithdae, tree).
nestingOf(threskiornithdae, ground).
nestingOf(pelecaniformes, ground).
nestingOf(pelecaniformes, tree).
%//////////////////////////////////////////////////////////////////////////////////////////

%//////////////////////////////////////////////////////////////////////////////////////////
%behavior(?A, ?B).
%behavior is a predicate that will return true if B is the behavior of A, where A is a compound species name, genus, family, or order but not a raw name.

behavior(X,Y):- var(X) -> hasCompoundName(_,_,X), behaviorOf(X,Y).
behavior(X,Y):- atom(X) -> behaviorOf(X,Y).

%this is where we store the behaviors of different pelicans, we use this in our behavior predicate.

behaviorOf(pelecanus_erythrorhynchos, surfaceDive).
behaviorOf(pelecanus_occidentalis, aerialDive).
behaviorOf(botaurus_lentiginosus, stalking).
behaviorOf(ixobrychus_exilis, stalking).
behaviorOf(ardea_herodias, stalking).
behaviorOf(ardea_alba, stalking).
behaviorOf(egretta_thula, stalking).
behaviorOf(egretta_caerulea, stalking).
behaviorOf(egretta_tricolor, stalking).
behaviorOf(egretta_rufescens, stalking).
behaviorOf(bubulcus_ibis, groundForager).
behaviorOf(butorides_virescens, stalking).
behaviorOf(nycticorax_nycticorax, stalking).
behaviorOf(nyctanassa_violacea, stalking).
behaviorOf(eudocimus_albus, probing).
behaviorOf(plegadis_falcinellus, probing).
behaviorOf(plegadis_chihi, probing).
behaviorOf(platalea_ajaja, probing).
behaviorOf(pelecanus, surfaceDive).
behaviorOf(pelecanus, aerialDive).
behaviorOf(pelecanidae, surfaceDive).
behaviorOf(ardeidae, stalking).
behaviorOf(ardeidae, groundForager).
behaviorOf(botaurus, stalking).
behaviorOf(ixobrychus, stalking).
behaviorOf(ardea, stalking).
behaviorOf(egretta, stalking).
behaviorOf(bubulcus, groundForager).
behaviorOf(butorides, stalking).
behaviorOf(nycticorax, stalking).
behaviorOf(nyctanassa, stalking).
behaviorOf(threskiornithdae, probing).
behaviorOf(eudocimus, probing).
behaviorOf(plegadis, probing).
behaviorOf(platalea, probing).
behaviorOf(pelecaniformes, surfaceDive).
behaviorOf(pelecaniformes, aerialDive).
behaviorOf(pelecaniformes, probing).
behaviorOf(pelecaniformes, stalking).
behaviorOf(pelecaniformes, groundForager).
%//////////////////////////////////////////////////////////////////////////////////////////

%//////////////////////////////////////////////////////////////////////////////////////////
%conservation(?A, ?B).
%conservation is a predicate that will return true if B is the conservation status of A, A may be a compound species name, genus, family, or order, but may not be a raw species name.

conservation(X,Y):- var(X) -> hasCompoundName(_,_,X), conservationStatus(X,Y).
conservation(X,Y):- atom(X) -> conservationStatus(X,Y).


%conservationStatus is where we store the conservation statuses of different pelicans, we use this in our conservation 

conservationStatus(pelecanus_erythrorhynchos, lc).
conservationStatus(pelecanus_occidentalis, lc).
conservationStatus(botaurus_lentiginosus, lc).
conservationStatus(ixobrychus_exilis, lc).
conservationStatus(ardea_herodias, lc).
conservationStatus(ardea_alba, lc).
conservationStatus(egretta_thula, lc).
conservationStatus(egretta_caerulea, lc).
conservationStatus(egretta_tricolor, lc).
conservationStatus(egretta_rufescens, nt).
conservationStatus(bubulcus_ibis, lc).
conservationStatus(butorides_virescens, lc).
conservationStatus(nycticorax_nycticorax, lc).
conservationStatus(nyctanassa_violacea, lc).
conservationStatus(eudocimus_albus, lc).
conservationStatus(plegadis_falcinellus, lc).
conservationStatus(plegadis_chihi, lc).
conservationStatus(platalea_ajaja, lc).
conservationStatus(pelecanus, lc).
conservationStatus(botaurus, lc).
conservationStatus(ixobrychus, lc).
conservationStatus(ardea, lc).
conservationStatus(egretta, lc).
conservationStatus(egretta, nt).
conservationStatus(bubulcus, lc).
conservationStatus(butorides, lc).
conservationStatus(nycticorax, lc).
conservationStatus(nyctanassa, lc).
conservationStatus(eudocimus, lc).
conservationStatus(plegadis, lc).
conservationStatus(platalea, lc).
conservationStatus(pelecanidae, lc).
conservationStatus(ardeidae, lc).
conservationStatus(ardeidae, nt).
conservationStatus(threskiornithdae, lc).
conservationStatus(pelecaniformes, lc).
conservationStatus(pelecaniformes, nt).
%//////////////////////////////////////////////////////////////////////////////////////////
