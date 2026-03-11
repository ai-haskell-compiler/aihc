# Extracted from
#    http://www.ninebynine.org/RDFNotes/Swish/Intro.html
#    09 April 2011
#
# it is the same as the script in
#    http://www.ninebynine.org/Software/Swish-0.2.0.html
# bar some minor formatting differences.
#
# -- Example Swish script --
#
# Comment lines start with a '#'
#
# The script syntax is loosely based on Notation3, but it is a quite
# different language, except that embedded graphs (enclosed in {...})
# are encoded using Notation3 syntax.
#
# -- Prefix declarations --
#
# As well as being used for all labels defined and used by the script
# itself, these are applied to all graph expressions within the script
# file, and to graphs created by scripted inferences,
# but are not applied to any graphs read in from an external source.

# NOTE: the automatic prefix declarations are no-longer provided by
#       Swish
#
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdfd:  <http://id.ninebynine.org/2003/rdfext/rdfd#> .

@prefix ex:  <http://id.ninebynine.org/wip/2003/swishtest/> .
@prefix pv:  <http://id.ninebynine.org/wip/2003/swishtest/pv/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix xsd_integer: <http://id.ninebynine.org/2003/XMLSchema/integer#> .
@prefix rs_rdf:  <http://id.ninebynine.org/2003/Ruleset/rdf#> .
@prefix rs_rdfs: <http://id.ninebynine.org/2003/Ruleset/rdfs#> .
@prefix :   <http://id.ninebynine.org/default/> .

# -- Simple named graph declarations --

ex:Rule01Ant :- { ?p ex:son ?o . }

ex:Rule01Con :- { ?o a ex:Male ; ex:parent ?p . }

ex:TomSonDick :- { :Tom ex:son :Dick . }

ex:TomSonHarry :- { :Tom ex:son :Harry . }


# -- Named rule definition --

@rule ex:Rule01 :- ( ex:Rule01Ant ) => ex:Rule01Con


# -- Named ruleset definition --
#
# A 'ruleset' is a collection of axioms and rules.
#
# Currently, the ruleset is identified using the namespace alone;
# i.e. the 'rules' in 'ex:rules' below is not used.
# This is under review.

@ruleset ex:rules :- (ex:TomSonDick ex:TomSonHarry) ; (ex:Rule01)

# -- Forward application of rule --
#
# The rule is identified here by ruleset and a name within the ruleset.

@fwdchain ex:rules ex:Rule01 { :Tom ex:son :Charles . } => ex:Rule01fwd

# -- Compare graphs --
#
# Compare result of inference with expected result.
# This is a graph isomorphism test rather than strict equality,
# to allow for bnode renaming.
# If the graphs are not equal, a message is generated
# The comment (';' to end of line) is included in any message generated

ex:ExpectedRule01fwd :- { :Charles a ex:Male ; ex:parent :Tom . }

@asserteq ex:Rule01fwd ex:ExpectedRule01fwd
   ; Infer that Charles is male and has parent Tom

# -- Display graph --
#
# Write graph as Notation3 to standard output.
# The comment is included in the output.

@write ex:Rule01fwd ; Charles is male and has parent Tom

# -- Write graph to file --
#
# The comment is included at the head of the file.
# (TODO: support for output to Web using HTTP.)

@write ex:Rule01fwd <Example1.n3> ; Charles is male and has parent Tom

# -- Read graph from file --
#
# Creates a new named graph in the Swish environment.
# (TODO: support for input from Web using HTTP.)

@read ex:Rule01inp <Example1.n3>

# -- Proof check --
#
# This proof uses the built-in RDF and RDFS rulesets,
# which are the RDF- and RDFS- entailment rules described in the RDF
# formal semantics document.
#
# To prove:
#     ex:foo ex:prop "a" .
# RDFS-entails
#     ex:foo ex:prop _:x .
#     _:x rdf:type rdfs:Resource .
#
# If the proof is not valid according to the axioms and rules of the
# ruleset(s) used and antecedents given, then an error is reported
# indicating the failed proof step.

ex:Input01 :- { ex:foo ex:prop "a" . }

ex:Result :- { ex:foo ex:prop _:a . _:a rdf:type rdfs:Resource . }

# This is the version from
#    http://www.ninebynine.org/RDFNotes/Swish/Intro.html#ScriptExample
# which does not work. It appears that ex:Step01c can not be proved,
# so we split it into two steps in the version below.
#
#@proof ex:Proof01 ( rs_rdf:rules rs_rdfs:rules )
#  @input  ex:Input01
#  @step   rs_rdfs:r3 ( rs_rdfs:a10 rs_rdfs:a39 )
#          => ex:Step01a :- { rdfs:Literal rdf:type rdfs:Class . }
#  @step   rs_rdfs:r8 ( ex:Step01a )
#          => ex:Step01b :- { rdfs:Literal rdfs:subClassOf rdfs:Resource . }
#  @step   rs_rdfs:r1 ( ex:Input01 )
#          => ex:Step01c :- { ex:foo ex:prop _:a . _:a rdf:type rdfs:Literal . }
#  @step   rs_rdfs:r9 ( ex:Step01b ex:Step01c )
#          => ex:Step01d :- { _:a rdf:type rdfs:Resource . }
#  @step   rs_rdf:se  ( ex:Step01c ex:Step01d )   => ex:Result
#  @result ex:Result

@proof ex:Proof01 ( rs_rdf:rules rs_rdfs:rules )
  @input  ex:Input01
  @step   rs_rdfs:r3 ( rs_rdfs:a10 rs_rdfs:a39 )
          => ex:Step01a :- { rdfs:Literal rdf:type rdfs:Class . }
  @step   rs_rdfs:r8 ( ex:Step01a )
          => ex:Step01b :- { rdfs:Literal rdfs:subClassOf rdfs:Resource . }
  @step   rs_rdf:lg ( ex:Input01 )
          => ex:Step01c1 :- { ex:foo ex:prop _:a . _:a rdf:_allocatedTo "a" . }
  @step   rs_rdfs:r1 ( ex:Step01c1 )
          => ex:Step01c2 :- { _:a rdf:type rdfs:Literal . }
  @step   rs_rdfs:r9 ( ex:Step01b ex:Step01c2 )
          => ex:Step01d :- { _:a rdf:type rdfs:Resource . }
  @step   rs_rdf:se  ( ex:Step01c1 ex:Step01c2 ex:Step01d )   => ex:Result
  @result ex:Result

# -- Restriction based datatype inferencing --
#
# Datatype inferencing based on a general class restriction and
# a predefined relation (per idea noted by Pan and Horrocks).

ex:VehicleRule :-
  { :PassengerVehicle a rdfd:GeneralRestriction ;
      rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ;
      rdfd:constraint xsd_integer:sum ;
      rdfd:maxCardinality "1"^^xsd:nonNegativeInteger . }

# Define a new ruleset based on a declaration of a constraint class
# and reference to built-in datatype.
# The datatype constraint xsd_integer:sum is part of the definition
# of datatype xsd:integer that is cited in the constraint ruleset
# declaration.  It relates named properties of a class instance.

@constraints pv:rules :- ( ex:VehicleRule ) | xsd:integer

# Input data for test cases:

ex:Test01Inp :-
  { _:a1 a :PassengerVehicle ;
      :seatedCapacity "30"^^xsd:integer ;
      :standingCapacity "20"^^xsd:integer . }

# Forward chaining test case:

ex:Test01Fwd :- { _:a1 :totalCapacity "50"^^xsd:integer . }

@fwdchain pv:rules :PassengerVehicle ex:Test01Inp => :t1f
@asserteq :t1f ex:Test01Fwd  ; Forward chain test

# Backward chaining test case:
#
# Note that the result of backward chaining is a list of alternatives,
# any one of which is sufficient to derive the given conclusion.

ex:Test01Bwd0 :-
  { _:a1 a :PassengerVehicle .
    _:a1 :totalCapacity "50"^^xsd:integer .
    _:a1 :seatedCapacity "30"^^xsd:integer . }

ex:Test01Bwd1 :-
  { _:a1 a :PassengerVehicle .
    _:a1 :totalCapacity "50"^^xsd:integer .
    _:a1 :standingCapacity "20"^^xsd:integer . }

# Declare list of graphs:

ex:Test01Bwd :- ( ex:Test01Bwd0 ex:Test01Bwd1 )

@bwdchain pv:rules :PassengerVehicle ex:Test01Inp <= :t1b

@asserteq :t1b ex:Test01Bwd  ; Backward chain test

# Can test for graph membership in a list

@assertin ex:Test01Bwd0 :t1b ; Backward chain component test (0)
@assertin ex:Test01Bwd1 :t1b ; Backward chain component test (1)

# -- Merge graphs --
#
# Merging renames bnodes to avoid collisions.

@merge ( ex:Test01Bwd0 ex:Test01Bwd1 ) => ex:Merged

# This form of comparison sets the Swish exit status based on the result.

ex:ExpectedMerged :-
  { _:a1 a :PassengerVehicle .
    _:a1 :totalCapacity "50"^^xsd:integer .
    _:a1 :seatedCapacity "30"^^xsd:integer .
    _:a2 a :PassengerVehicle .
    _:a2 :totalCapacity "50"^^xsd:integer .
    _:a2 :standingCapacity "20"^^xsd:integer . }

@compare ex:Merged ex:ExpectedMerged

# End of example script

