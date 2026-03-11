# $Id: SwishTest.ss,v 1.8 2003/12/18 20:46:24 graham Exp $
# 
# Swish script: test script 
#
# --------+---------+---------+---------+---------+---------+---------+---------

@prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#> .

@prefix ex:  <http://id.ninebynine.org/wip/2003/swishtest/> .
@prefix pv:  <http://id.ninebynine.org/wip/2003/swishtest/pv/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix xsd_integer: <http://id.ninebynine.org/2003/XMLSchema/integer#> .
@prefix rdfd:    <http://id.ninebynine.org/2003/rdfext/rdfd#> .
@prefix rs_rdf:  <http://id.ninebynine.org/2003/Ruleset/rdf#> .
@prefix rs_rdfs: <http://id.ninebynine.org/2003/Ruleset/rdfs#> .
@prefix :   <http://id.ninebynine.org/default/> .


# Simple inference tests

ex:VehicleRule :-
  { 
    :PassengerVehicle a rdfd:GeneralRestriction ;
      rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ;
      rdfd:constraint xsd_integer:sum .
  }

ex:VehicleRule1 :-
  { 
    :PassengerVehicle1 a rdfd:GeneralRestriction ;
      rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ;
      rdfd:constraint xsd_integer:sum ;
      rdfd:maxCardinality "1"^^xsd:nonNegativeInteger .
  }

@merge ( ex:VehicleRule ex:VehicleRule1 ) => ex:VehicleRules
@write ex:VehicleRules <data/VehicleRules.n3> ; Vehicle rules file
@read  ex:VehicleRuleFile <data/VehicleRules.n3>
@asserteq ex:VehicleRuleFile ex:VehicleRules ; Compare read and internal graphs

ex:Test01Inp :-
  { _:a1 a :PassengerVehicle ;
      :seatedCapacity "30"^^xsd:integer ;
      :standingCapacity "20"^^xsd:integer .
  }

ex:Test01Fwd :-
  { _:a1 :totalCapacity "50"^^xsd:integer .
  }

ex:Test01Bwd0 :-
  { _:a1 a :PassengerVehicle .
    _:a1 :totalCapacity "50"^^xsd:integer .
    _:a1 :seatedCapacity "30"^^xsd:integer .
  }

ex:Test01Bwd1 :-
  { _:a1 a :PassengerVehicle .
    _:a1 :totalCapacity "50"^^xsd:integer .
    _:a1 :standingCapacity "20"^^xsd:integer .
  }

ex:Test01Bwd :- ( ex:Test01Bwd0 ex:Test01Bwd1 )

@constraints pv:rules :- ( ex:VehicleRule ex:VehicleRule1 ) | xsd:integer

@fwdchain pv:rules :PassengerVehicle ex:Test01Inp => :t1f
# @write :t1f ; Forward chain result :t1f
@asserteq :t1f ex:Test01Fwd  ; Forward chain test

@bwdchain pv:rules :PassengerVehicle ex:Test01Inp <= :t1b
# @write :t1b <data/a1.n3> ; Backward chain result :t1b
# @write ex:Test01Bwd <data/a2.n3> ; Backward chain expected ex:Test01Bwd
@asserteq :t1b ex:Test01Bwd  ; Backward chain test
@assertin ex:Test01Bwd0 :t1b ; Backward chain component test (0)
@assertin ex:Test01Bwd1 :t1b ; Backward chain component test (1)

# Proof test, using simple built-in RDF ruleset
#
# To prove:
#     ex:foo ex:prop "a" .
# RDFS-entails
#     ex:foo ex:prop _:x .
#     _:x rdf:type rdfs:Resource .
#

ex:Input01 :-
    { ex:foo ex:prop "a" .
    }

ex:Step01a :-
    { rdfs:Literal rdf:type rdfs:Class .
    }

ex:Step01b :-
    { rdfs:Literal rdfs:subClassOf rdfs:Resource .
    }

ex:Step01c :-
    { ex:foo ex:prop _:a .
      _:a rdf:_allocatedTo "a" .
    }

ex:Step01d :-
    { _:a rdf:type rdfs:Literal .
    }

ex:Step01e :-
    { _:a rdf:type rdfs:Resource .
    }

ex:Result :-
    { ex:foo ex:prop _:a .
      _:a rdf:type rdfs:Resource .
    }

@proof ex:Proof01 ( rs_rdf:rules rs_rdfs:rules )
  @input  ex:Input01
  @step   rs_rdfs:r3 ( rs_rdfs:a10 rs_rdfs:a39 ) => ex:Step01a
  @step   rs_rdfs:r8 ( ex:Step01a )              => ex:Step01b
  @step   rs_rdf:lg  ( ex:Input01 )              => ex:Step01c
  @step   rs_rdfs:r1 ( ex:Step01c )              => ex:Step01d
  @step   rs_rdfs:r9 ( ex:Step01b ex:Step01d )   => ex:Step01e
  @step   rs_rdf:se  ( ex:Step01c ex:Step01e )   => ex:Result
  @result ex:Result

#@fwdchain rs_rdfs:rules rs_rdfs:r9 ( ex:Step01b ex:Step01c ) => ex:Step01dd
#@write ex:Step01b  ; ex:Step01b
#@write ex:Step01c  ; ex:Step01c
#@write ex:Step01dd ; Forward chain simple rule rs_rdfs:r9


# Simple deduction rule test

ex:Rule01Ant :-
    { ?p ex:son ?o . }
ex:Rule01Con :-
    { ?o a ex:Male ;
         ex:parent ?p . }

ex:Rule02Ant :-
    { ?p ex:daughter ?o . }
ex:Rule02Con :-
    { ?o a ex:Female ;
         ex:parent ?p . }

ex:Rule03Ant :-
    { ?o1 a ex:Male ;
          ex:parent ?p .
      ?o2 a ex:Female ;
          ex:parent ?p .
    }
ex:Rule03Con :-
    { ?o1 ex:sister  ?o2 .
      ?o2 ex:brother ?o1 .
    }

@rule ex:Rule01 :- ( ex:Rule01Ant ) => ex:Rule01Con
@rule ex:Rule02 :- ( ex:Rule02Ant ) => ex:Rule02Con
@rule ex:Rule03 :- ( ex:Rule03Ant ) => ex:Rule03Con

@ruleset ex:rules :- () ; ( ex:Rule01 ex:Rule02 ex:Rule03 )

@proof ex:Proof02 ( ex:rules )
  @input  ex:inp :- { _:p ex:son ex:s ; ex:daughter ex:d . }
  @step   ex:Rule01 ( ex:inp ) => ex:st1 :- 
            { ex:s a ex:Male ; ex:parent _:a . }
  @step   ex:Rule02 ( ex:inp ) => ex:st2 :- 
            { ex:d a ex:Female ; ex:parent _:a . }
  @step   ex:Rule03 ( ex:st1 ex:st2 ) => ex:res :-
            { ex:s ex:sister  ex:d .
              ex:d ex:brother ex:s . }
  @result ex:res

#ex:proof01inp :- { _:p ex:son ex:s ; ex:daughter ex:d . }
#@fwdchain ex:rules ex:Rule01 ex:proof01inp => ex:rule01fwd
#@write ex:rule01fwd ; Forward chain simple rule 01
#@fwdchain ex:rules ex:Rule02 ex:proof01inp => ex:rule02fwd
#@write ex:rule02fwd ; Forward chain simple rule 02
#@fwdchain ex:rules ex:Rule03 (ex:rule01fwd ex:rule02fwd) => ex:rule03fwd
#@write ex:rule03fwd ; Forward chain simple rule 03

# TODO:  test rule with variable binding modifiers

# Merge, I/O and compare tests

ex:TestMerge :-
  { _:b1 a :PassengerVehicle .
    _:b1 :totalCapacity "50"^^xsd:integer .
    _:b1 :seatedCapacity "30"^^xsd:integer .
    _:b2 a :PassengerVehicle .
    _:b2 :totalCapacity "50"^^xsd:integer .
    _:b2 :standingCapacity "20"^^xsd:integer .
  }

@merge ( ex:Test01Bwd0 ex:Test01Bwd1 ) => ex:tmout

@asserteq ex:TestMerge ex:tmout ; Check merged graph

@write ex:tmout <data/TestMerge.n3m> ; Test graph merge and read/write
@write ex:TestMerge <data/TestMerge.n3> ; Test graph merge and read/write

@read  ex:tmin  <data/TestMerge.n3>

@asserteq ex:TestMerge ex:tmin ; Check graph read back

@compare ex:tmin ex:tmout
# @compare ex:tmin ex:VehicleRule

# $Log: SwishTest.ss,v $
# Revision 1.8  2003/12/18 20:46:24  graham
# Added xsd:string module to capture equivalence of xsd:string
# and plain literals without a language tag
#
# Revision 1.7  2003/12/11 19:11:07  graham
# Script processor passes all initial tests.
#
# Revision 1.6  2003/12/10 14:43:00  graham
# Backup.
#
# Revision 1.5  2003/12/10 03:48:58  graham
# SwishScript nearly complete:  BwdChain and PrrofCheck to do.
#
# Revision 1.4  2003/12/08 23:55:36  graham
# Various enhancements to variable bindings and proof structure.
# New module BuiltInMap coded and tested.
# Script processor is yet to be completed.
#
# Revision 1.3  2003/12/05 02:31:32  graham
# Script parsing complete.
# Some Swish script functions run successfully.
# Command execution to be completed.
#
# Revision 1.2  2003/12/04 02:53:28  graham
# More changes to LookupMap functions.
# SwishScript logic part complete, type-checks OK.
#
# Revision 1.1  2003/12/01 18:51:38  graham
# Described syntax for Swish script.
# Created Swish scripting test data.
# Edited export/import lists in Swish main program modules.
#
