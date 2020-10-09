def find_failed_student(student_marks):
    failed_marks = 0
    for mark in student_marks:
        if mark <=40:
            failed_marks +=1
    return failed_marks

def above_given_marks(student_marks, x):
    above_marks = []
    for mark in student_marks:
        if mark < x:
            continue
        above_marks.append(mark)
    return above_marks

def first_k_student_marks(student_marks, k):
    i = 0
    marks = []
    while i <= k:
        marks.append(student_marks[i])
    return marks

def centum_scorer(student_info):
    for name, mark in student_info.items():
        if mark == 100:
            return name
    else:
        return "No Centums"
