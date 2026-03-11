# $Id: VehicleCapacity.ss,v 1.1 2004/02/09 22:22:44 graham Exp $
# 
# Swish script: vehicle capacity examples
#
# --------+---------+---------+---------+---------+---------+---------+---------

@prefix ex:  <http://id.ninebynine.org/wip/2003/swishtest/> .
@prefix pv:  <http://id.ninebynine.org/wip/2003/swishtest/pv/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix xsd_integer: <http://id.ninebynine.org/2003/XMLSchema/integer#> .
@prefix rdfd:    <http://id.ninebynine.org/2003/rdfext/rdfd#> .
@prefix :   <http://id.ninebynine.org/default/> .


# Deduce total capacity using simple deduction with variable binding modifier

ex:Test01Inp :-
  { _:a1 a :PassengerVehicle ;
      :seatedCapacity "98"^^xsd:integer ;
      :standingCapacity "12"^^xsd:integer .
  }

ex:Rule01Ant :-
  { _:a1 a :PassengerVehicle ;
      :seatedCapacity   ?c1 ;
      :standingCapacity ?c2 .
  }
ex:Rule01Con :-
  { _:a1 :totalCapacity ?ct .
  }

@rule ex:Rule1 :- ( ex:Rule01Ant ) => ex:Rule01Con 
                    | ( xsd_integer:sum ?ct ?c1 ?c2 )

@ruleset pv:rules1 :- () ; ( ex:Rule1 )

@fwdchain pv:rules1 ex:Rule1 ex:Test01Inp => :t1f
@write :t1f ; Forward chain result :t1f


# Deduce total capacity using general restriction

ex:VehicleRule2 :-
  { :PassengerVehicle a rdfd:GeneralRestriction ;
      rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ;
      rdfd:constraint xsd_integer:sum ;
      rdfd:maxCardinality "1"^^xsd:nonNegativeInteger .
  }

@constraints pv:rules2 :- ( ex:VehicleRule2 ) | xsd:integer

@fwdchain pv:rules2 :PassengerVehicle ex:Test01Inp => :t2f
@write :t2f ; Forward chain result :t2f

@bwdchain pv:rules2 :PassengerVehicle ex:Test01Inp <= :t2b
@write :t2b ; Backward chain result :t2b

# $Log: VehicleCapacity.ss,v $
# Revision 1.1  2004/02/09 22:22:44  graham
# Graph matching updates:  change return value to give some indication
# of the extent match achieved in the case of no match.
# Added new module GraphPartition and test cases.
# Add VehicleCapcity demonstration script.
#
