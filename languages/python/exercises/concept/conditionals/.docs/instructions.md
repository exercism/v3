<!-- 
Date: 26 Jan 2021
This is a draft to see if this is question theme is suitable or not and will change according to 
review feedback
 -->

In this exercise we will develop a simple control system for a nuclear reactor.

In order for reactor to produce power it must be in a state of criticality. 
If the the reactor becomes less than criticality it can damage the reactor. 
If it goes beyond criticalty it can result in a meltdown.

[//]: # 'Check for criticality'
<!-- to teach base if condition -->

## 1. Check for criticality

First thing a control system must do is to check if the reactor is critical. 
A reactor is said to be critical if it satisfies the following parameters
temprature < 800
Number of neutrons emitted per second > 500
and product of temprature and neutrons emitted per second less than 50000

impliment a function called `is_critical` that takes in 2 arguments the temprature and neutrons emitted per second and return True or False 

```python

>>>is_critical(750,600):
True
```

[//]: # 'Determine the Power output band'

<!-- For teaching elif -->

## 2. Determine the Power output range

Now that the reactor has started producing power we need to determine the efficency of the reactor.
The efficency can be defined into 3 zones.
green -> 80-100% efficency
orange -> 60-80% efficency
red -> 30 - 60% efficency
black -> <30% efficent


efficency is calculated as (generated power/ theoretical max power)*100
where generated power = voltage * current

impliment a function called `reactor_efficency` that takes in 3 arguments the voltage, current and theroretical max power and outputs the efficency band 'green','orange' or 'red'

```python
>>> reactor_efficency(200,50,1500)
'orange'
```



[//]: # 'Fail Safe Mechanism'

<!-- Intention is to teach use of if, elif and else -->

## 3. Fail Safe Mechanism

The final part involves creating a fail safe mechanism for the reactor. We can increase/decrease or stop 
the criticality state of a reactor by inserting control rods into the reactor. 

Impliment a function called `fail_safe` which takes in 3 parameters temprature, nutrons produced per second and threshold to outputs status codes.

if temprature * nutrons per second < 40% of threshold output status code of 100 indicating that
the control rods must be removed to make it go critical and produce power

if temprature * nutrons per second is with in plus or minus 10% of threshold the reactor is in criticality
and outputs a status code of 200, indicating that the reactor is in optimum condition and control rods 
are in idea position

If temperature * neutron per second is not in the above stated ranges. The reactor is going into meltdown
and a status code of 500 must be passed to immediatly shutdown the reactor 