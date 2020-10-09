You are a teacher and you are correcting papers of your students who wrote an exam you just conducted. You are tasked with the following activities.

## 1. Failed Students

Given the function `find_failed_student()` that takes `student_marks` as parameter. Find the Number of students who have failed the subject. We say that a student has failed if his mark is below or equal to 40.

Note: `Iterate` through the student marks to find out your answer.
​
Result should be an `int`.

```python
>>> find_failed_student(student_marks=[90,40,55,70,30]))
2
```

## 2. Above x marks

The Headmaster wants to find out the toppers of the students. The topper values fluctate and you need to find the list of marks that are more than the given value.

Given the function `above_given_marks()` where `student_marks and x` are 2 parameters

1. Student marks are a list of marks of each student
2. X - return the marks which are more than or equal to x. ie: return if mark >= x.

Note: If you find a mark which is less than x, you should continue to the next mark.​

```python
>>> above_given_marks(student_marks=[90,40,55,70,30], 70)
[90, 70]
```

## 3. First K Students.

Given the function `first_k_student_marks()` with parameters `student_marks and k`

1. Student marks are a list of marks of each student
2. k is the number of students we need to return.

You need to return the first K number of student Marks. Once you reach K number of students, you should exit out of the loop.

```python
>>> first_k_student_marks(student_marks=[90,80,100], k=1)
[90]
```

## 4. Centum Scorer

Given the function `centum_scorer()` with parameters `student_info`.
Student Info is a dictionary containing name and mark of the student `{"Charles": 90, "Tony": 80}`

Find if we have any students who has scored centum in the exam. A Centum in latin means `100`. If we dont find a Centum Scorer in the list, then return "No Centums"

Return the first centum scorer.

```python
>>> centum_scorer(student_info={"Charles": 90, "Tony": 80, "Alex":100})
Alex
>>> centum_scorer(student_info={"Charles": 90, "Tony": 80})
No Centums
```
