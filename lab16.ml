(*
                             CS51 Lab 16
                     Object-Oriented Programming
*)

(*
                               SOLUTION
*)

(* In this lab, you'll use a Point module for x-y points, provided in
the files point.ml and point.mli. You may want to look over point.mli
to get a sense of the interface. (There's no need to look at
point.ml. Why?) *)

open Point ;;

(*====================================================================
Part 1 - Functional vehicles 

In this part, you will revisit algebraic data types to model some
vehicles, implementing functionality in a function-oriented approach,
with comparison to object-oriented implementations to come in Part 2.

We start with a vehicle type that can be a Bus, a Car or a Truck, each
constructed of a point that represents its current x-y position and a
float that represents the energy stored in the tank (measured in units
of gallons of gas, a useful unit even for electric vehicles). *)
  
type vehicle = 
  | Bus of point * float
  | Car of point * float
  | Truck of point * float ;;

(*....................................................................
Exercise 1: Define a function get_efficiency that takes a vehicle and
returns its efficiency in miles per gallon (mpg). For purposes here,
buses get 20. mpg, cars get 30. mpg, and trucks get 15. mpg. (Notice
that these values are floats.)
....................................................................*)
   
let get_efficiency (v : vehicle) : float = 
  match v with 
  | Bus _-> 20.
  | Car _ -> 30. 
  | Truck _ -> 15. ;; 
     
(*....................................................................
Exercise 2: Write a function get_energy that returns the amount of
energy a vehicle has available.
....................................................................*)
   
let get_energy (veh : vehicle) : float =
  match veh with 
  | Bus (_, energy) 
  | Car (_, energy) 
  | Truck (_, energy) -> energy ;; 

(*....................................................................
Exercise 3: Write a function get_pos that returns the x-y position of
a vehicle as a point.
....................................................................*)
   
let get_pos (veh : vehicle) : point = 
  match veh with
  | Bus (p, _)
  | Car (p, _)
  | Truck (p, _) -> p ;;
     
(*....................................................................
Exercise 4: Let's define a function that allows these vehicles to
travel somewhere. Write a go function that takes a vehicle and a
distance (a float) and a direction (an angle in radians represented by
a float), and updates the vehicle's position tuple to travel that
distance in that direction, and reduces its energy
accordingly. (Fortunately, you don't need to know how to do the
calculation of the change in position, since the Point module can
handle it for you; see the Point.offset function.) Assume that the
vehicle has a full tank, and that each vehicle can only go as far as
its energy will carry it, so if the distance is farther than that, the
vehicle will go only as far as its energy allows. Your function should
return a new vehicle with the updated position and energy. (Calls with
negative distance should raise an Invalid_argument exception.)
....................................................................*)

let go (veh : vehicle) (distance : float) (angle : float) : vehicle =
  if distance < 0. then
    raise (Invalid_argument "go: can't move in reverse")
  else
    let energy = get_energy veh in 
    let efficiency = get_efficiency veh in
    let p = get_pos veh in
    let distance = min distance (energy *. efficiency) in
    let new_p = offset p distance angle in
    let new_energy = energy -. distance /. efficiency in
    match veh with 
    | Bus _ -> Bus (new_p, new_energy) 
    | Car _ -> Car (new_p, new_energy)
    | Truck _ -> Truck (new_p, new_energy) ;;

(*====================================================================
Part 2: Object-oriented vehicles

After implementing a few functions to be performed on vehicles defined
as an algebraic data type, you might be getting weary of all these
match statements you need to add. Consider what would happen if we
added a new vehicle -- say, a motorcycle -- to the mix? We update the
variant type definition of a vehicle to include motorcycles simply
enough, but suddenly we get a bunch of nonexhaustive match cases in
all the functions we implemented matching on vehicles. This in itself
is a source of motivation to pursue a better strategy. This example is
a small case, but consider what would happen if you had dozens of
instances in your code where you match to the vehicle variant
type. For each of these different locations, perhaps even in different
files that you have never seen or touched (if working in a group), the
extra match cases would need to be added.

We named our type "vehicle". What do vehicles have in common? In our
case, they have a certain amount of energy, an efficiency with which
they use that energy, and the ability to move in two dimensional
space. We can provide that functionality as a class. When we need an
actual instance of that class, we can create an object based upon its
structure. We've provided the skeleton of a vehicle_class class
(called vehicle_class rather than vehicle because we already called
the algebraic data type vehicle and OCaml doesn't allow the shared
name). Notice the syntax for defining a class, as well as the field
variables and methods.

Below, we provide the start of an implementation of a vehicle class,
which takes several arguments:

  capacity -- the maximum amount of energy (in gallons of gas) that
  can be stored in the vehicle's tank/battery

  energy -- the vehicle's initial amount of energy  

  efficiency -- the vehicle's efficiency in mpg

  initial_pos -- the initial position of the vehicle

The class has several instance variables to store these values, as
well as an odometer field (initially 0.) to track the total number of
miles driven.

(Note that some fields are mutable while some are not. Do you see
why?) *)

class vehicle_class (capacity: float)
                    (efficiency : float)
                    (initial_energy : float)
                    (initial_pos : point) =
  object
    val capacity = capacity
    val mutable energy = initial_energy
    val efficiency = efficiency
    val mutable pos = initial_pos
    val mutable odometer = 0.
                             
    (*................................................................
    Exercise 5: To this vehicle class, add methods "get_distance",
    "get_pos", and "get_energy" which return the current distance
    traveled, position, and remaining energy, respectively.
    ................................................................*)

    method get_distance : float = 
      odometer

    
    method get_pos : point = 
      pos
         
    
    method get_energy : float = 
      energy

    
    (*................................................................
    Exercise 6: Now add a method to your vehicle class called "go"
    which takes in a float for the distance the vehicle wants to
    travel and a float for the direction it should head. The actual
    distance traveled should be dependent on the energy available as
    in the implementation of the go function above. Instead of
    returning a new object, you should simply be updating the fields
    of the same object. Notice how this differs from the way you did
    it before.
    ................................................................*)

    
    method go (distance : float) (angle : float) : unit = 
      if distance < 0. then
        raise (Invalid_argument "go: can't move in reverse")
      else
        let distance = min distance (energy *. efficiency) in
        pos <- offset pos distance angle;
        energy <- energy -. distance /. efficiency;
        odometer <- odometer +. distance

    
    (*................................................................
    Exercise 7: Since we'll eventually run out of energy, it would be
    useful for a vehicle to recharge or fill the tank. Define a fill
    method class that resets the energy to whatever the vehicle's
    capacity is.
    ................................................................*)
   
    
    method fill : unit = 
      energy <- capacity
  
  end ;; 

(*====================================================================
Part 3 Inheritance

Let's say that we know certain facts about the energy and efficiency
corresponding to buses, trucks and cars as we did above and thus would
like to conveniently be able to refer to them in more detail than just
vehicles, for whatever purpose we may have down the road. Instead of
implementing a completely new class for each of the types of vehicles,
we can make use of code we already wrote in the vehicle_class
definition by taking advantage of inheritance. Try this out for
yourself below.

......................................................................
Exercise 8: Define a car class, taking advantage of inheritance. Cars
should have the same energy capacity and efficiency as described above
in Part 1.
....................................................................*)

class car (initial_energy : float) (initial_pos : point) = 
  object
    inherit vehicle_class 100. 30. initial_energy initial_pos
  end ;;

(*....................................................................
Exercise 9: Now, define a truck class similarly to the car class, but
with the truck's specifications given in Part 1.
....................................................................*)

class truck (initial_energy : float) (initial_pos : point) = 
  object
    inherit vehicle_class 150. 15. initial_energy initial_pos 
  end ;; 

(*....................................................................
Exercise 10: Finally, define the bus class. Rather than merely inherit
all the functionality from the vehicle class, we'll add additional
functionality that makes a bus a bus.

A bus should be able to pick up and drop off passengers. To implement
this functionality, give your bus class additional instance variables:
seats (the number of seats on the bus, which defines the maximum
number of passengers it can accommodate) and passengers (the number of
passengers currently on the bus). (You'll want to think about whether
these fields should be mutable or not.) The class should allow for
methods pick_up and drop_off, which both take the number of passengers
to perform the action on and return unit. Keep in mind you can't drop
off more passengers than you currently have, so in this case you can
just reset your passenger count to 0. A bus also offers finite seating
(50 seats, in our case) and this rule should be enforced as well. So,
if 70 people try to board the bus at the same time, only the first 50
will be able to.

Furthermore, when a bus goes in for a fill-up, it will behave as a
vehicle, but first needs to drop off all its passengers. Override the
fill method to implement this functionality.
....................................................................*)
 
class bus (initial_energy : float) (initial_pos : point) (seats : int) = 
  object (this)
    val mutable passengers = 0
    val seats = seats

    method get_passengers : int = passengers

    method get_seats : int = seats

    inherit vehicle_class 200. 20. initial_energy initial_pos as super
    
    method pick_up (n : int) : unit =
      passengers <- min seats (passengers + n)
   
    method drop_off (n : int) : unit =
      passengers <- max 0 (passengers - n)
    
    method! fill = 
      this#drop_off passengers;
      super#fill
  
  end ;;
