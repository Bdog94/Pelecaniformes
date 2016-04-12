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

hasCommonName(pelecanus_erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus_occidentalis, brownPelican).
hasCommonName(botaurus_lentiginosus, americanBittern).
hasCommonName(ixobrychus_exilis, leastBittern).
hasCommonName(ardea_herodias, greatBlueHeron).
hasCommonName(ardea_alba, greatEgret).
hasCommonName(egretta_thula, snowyEgret).
hasCommonName(egretta_caerulea, littleBlueHeron).
hasCommonName(egretta_tricolor, tricoloredHeron).
hasCommonName(egretta_rufescens, reddishEgret).
hasCommonName(bubulcus_ibis, cattleEgret).
hasCommonName(butorides_virescens, greenHeron).
hasCommonName(nycticorax_nycticorax, nycticorax).
hasCommonName(nyctanassa_violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus_albus, whiteIbis).
hasCommonName(plegadis_falcinellus, glossyIbis).
hasCommonName(plegadis_chihi, whiteFacedIbis).
hasCommonName(platalea_ajaja, roseateSpoonbill).
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


% just realized, we dont have to hardcode the rest after this comment

hasSciName(roeateSpoonbill, platalea_ajaja).
hasSciName(whiteFacedIbis, plegadis_chihi).
hasSciName(glossyIbis, plegadis_falcinellus).
hasSciName(whiteIbis, eudocimus_albus).
hasSciName(yellowCrownedNightHeron, nyctanassa_violacea).
hasSciName(blackCrownedNightHeron, nycticorax_nycticorax).
hasSciName(greenHeron, butorides_virescens).
hasSciName(cattleEgret, bubulcus_ibis).
hasSciName(reddishEgret, egretta_rufescens).
hasSciName(tricoloredHeron, egretta_tricolor).
hasSciName(littleBlueHeron, egretta_caerulea).
hasSciName(snowyEgret, egretta_thula).
hasSciName(greatEgret, ardea_alba).
hasSciName(greatBlueHeron, ardea_herodias).
hasSciName(leastBittern, ixobrychus_exilis).
hasSciName(americanBittern, botaurus_lentiginosus).
hasSciName(brownPelican, pelecanus_occidentalis).
hasSciName(americanWhitePelican, pelecanus_erythrorhynchos).
hasSciName(pelican, pelecanus).
hasSciName(bittern, botaurus).
hasSciName(bittern, ixobrychus).
hasSciName(heron, ardea).
hasSciName(heron, egretta).
hasSciName(egret, egretta).
hasSciName(egret, bubulcus).
hasSciName(heron, butorides).
hasSciName(nightHeron, nycticorax).
hasSciName(nightHeron, nyctanassa).
hasSciName(ibis, eudocimus).
hasSciName(ibis, plegadis).
hasSciName(spoonbill, platalea).

hasCompoundName(pelecanus, erythrorhynchos, pelecanus_erythrorhynchos).
hasCompoundName(pelecanus, occidentalis, pelecanus_occidentalis).
hasCompoundName(botaurus, lentiginosus, botaurus_lentiginosus).
hasCompoundName(ixobrychus, exilis, ixobrychus_exilis).
hasCompoundName(ardea, herodias, ardea_herodias).
hasCompoundName(ardea, alba, ardea_alba).
hasCompoundName(egretta, thula, egretta_thula).
hasCompoundName(egretta, caerulea, egretta_caerulea).
hasCompoundName(egretta, tricolor, egretta_tricolor).
hasCompoundName(egretta, rufescens, egretta_rufescens).
hasCompoundName(bubulcus, ibis, bubulcus_ibis).
hasCompoundName(butorides, virescens, butorides_virescens).
hasCompoundName(nycticorax, nycticorax, nycticorax_nycticorax).
hasCompoundName(nyctanassa, violacea, nyctanassa_violacea).
hasCompoundName(eudocimus, albus, eudocimus_albus).
hasCompoundName(plegadis, falcinellus, plegadis_falcinellus).
hasCompoundName(plegadis, chihi, plegadis_chihi).
hasCompoundName(platalea, ajaja, platalea_ajaja).


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

giveRaw(pelecanus_erythrorhynchos,erythrorhynchos). 
giveRaw(pelecanus_occidentalis,occidentalis). 
giveRaw(botaurus_lentiginosus,lentiginosus). 
giveRaw(ixobrychus_exilis,exilis). 
giveRaw(ardea_herodias,herodias). 
giveRaw(ardea_alba,alba). 
giveRaw(egretta_thula,thula). 
giveRaw(egretta_caerulea,caerulea). 
giveRaw(egretta_tricolor,tricolor). 
giveRaw(egretta_rufescens,rufescens). 
giveRaw(bubulcus_ibis,ibis). 
giveRaw(butorides_virescens,virescens). 
giveRaw(nycticorax_nycticorax,nycticorax). 
giveRaw(nyctanassa_violacea,violacea). 
giveRaw(eudocimus_albus,albus). 
giveRaw(plegadis_falcinellus,falcinellus). 
giveRaw(plegadis_chihi,chihi). 
giveRaw(platalea_ajaja,ajaja). 


% provideFull is a function that will give us the order, family, genus, species and common name of the species.
  
provideFull(pelecaniformes,pelecanidae,pelecanus,pelecanus_erythrorhynchos,americanWhitePelican). 
provideFull(pelecaniformes,pelecanidae,pelecanus,pelecanus_occidentalis,brownPelican). 
provideFull(pelecaniformes,ardeidae,botaurus,botaurus_lentiginosus,americanBittern). 
provideFull(pelecaniformes,ardeidae,ixobrychus,ixobrychus_exilis,leastBittern). 
provideFull(pelecaniformes,ardeidae,ardea,ardea_herodias,greatBlueHeron). 
provideFull(pelecaniformes,ardeidae,ardea,ardea_alba,greatEgret). 
provideFull(pelecaniformes,ardeidae,egretta,egretta_thula,snowyEgret). 
provideFull(pelecaniformes,ardeidae,egretta,egretta_caerulea,littleBlueHeron). 
provideFull(pelecaniformes,ardeidae,egretta,egretta_tricolor,tricoloredHeron). 
provideFull(pelecaniformes,ardeidae,egretta,egretta_rufescens,reddishEgret). 
provideFull(pelecaniformes,ardeidae,bubulcus,bubulcus_ibis,cattleEgret). 
provideFull(pelecaniformes,ardeidae,butorides,butorides_virescens,greenHeron). 
provideFull(pelecaniformes,ardeidae,nycticorax,nycticorax_nycticorax,blackCrownedNightHeron). 
provideFull(pelecaniformes,ardeidae,nyctanassa,nyctanassa_violacea,yellowCrownedNightHeron). 
provideFull(pelecaniformes,threskiornithdae,eudocimus,eudocimus_albus,whiteIbis). 
provideFull(pelecaniformes,threskiornithdae,plegadis,plegadis_falcinellus,glossyIbis). 
provideFull(pelecaniformes,threskiornithdae,plegadis,plegadis_chihi,whiteFacedIbis). 
provideFull(pelecaniformes,threskiornithdae,platalea,platalea_ajaja,roseateSpoonbill). 



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
habitat(X,Y):- var(X) -> hasCompoundName(_,_,X), habitatOf(X,Y).
habitat(X,Y):- atom(X) -> habitatOf(X,Y).

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
food(X,Y):- var(X) -> hasCompoundName(_,_,X), foodOf(X,Y).
food(X,Y):- atom(X) -> foodOf(X,Y).

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
nesting(X,Y):- var(X) -> hasCompoundName(_,_,X), nestingOf(X,Y).
nesting(X,Y):- atom(X) -> nestingOf(X,Y).

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
%behavior is a predicate that will provide us with the behavior of a compound species name, genus, family, or order but not a raw name.
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
%conservation is a predicate that will provide us with the conservation status of a compound species name, genus, family, or order, but not a raw name
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
