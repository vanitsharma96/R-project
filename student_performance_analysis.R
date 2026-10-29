# ============================================================
# STUDENT PERFORMANCE ANALYSIS USING R
# Beginner-friendly MCA project
# ============================================================

# 1. Create a sample dataset
students <- data.frame(
  RollNo = 101:110,
  Name = c("Aman", "Riya", "Rahul", "Neha", "Karan",
           "Simran", "Arjun", "Priya", "Manav", "Anjali"),
  Attendance = c(92, 95, 78, 88, 65, 90, 72, 98, 80, 85),
  English = c(78, 92, 65, 88, 70, 85, 60, 95, 74, 89),
  Mathematics = c(85, 96, 58, 90, 62, 88, 55, 98, 77, 84),
  Computer = c(90, 94, 72, 91, 68, 92, 64, 99, 81, 93)
)

# 2. Calculate total, average, result and grade
students$Total <- students$English + students$Mathematics + students$Computer
students$Average <- round(students$Total / 3, 2)
students$Result <- ifelse(students$Average >= 40, "Pass", "Fail")
students$Grade <- ifelse(students$Average >= 90, "A+",
                   ifelse(students$Average >= 80, "A",
                   ifelse(students$Average >= 70, "B",
                   ifelse(students$Average >= 60, "C",
                   ifelse(students$Average >= 40, "D", "F")))))

# 3. Display complete student report
cat("\\n===== STUDENT PERFORMANCE REPORT =====\\n")
print(students)

# 4. Summary statistics
cat("\\n===== CLASS SUMMARY =====\\n")
cat("Number of students:", nrow(students), "\\n")
cat("Class average:", round(mean(students$Average), 2), "\\n")
cat("Highest average:", max(students$Average), "\\n")
cat("Lowest average:", min(students$Average), "\\n")
cat("Students passed:", sum(students$Result == "Pass"), "\\n")
cat("Students failed:", sum(students$Result == "Fail"), "\\n")

# 5. Find the top-performing student
top_student <- students[which.max(students$Average), ]
cat("\\n===== TOP-PERFORMING STUDENT =====\\n")
print(top_student[, c("RollNo", "Name", "Average", "Grade")])

# 6. Subject-wise average marks
subject_averages <- data.frame(
  Subject = c("English", "Mathematics", "Computer"),
  AverageMarks = c(mean(students$English),
                   mean(students$Mathematics),
                   mean(students$Computer))
)
cat("\\n===== SUBJECT-WISE AVERAGE =====\\n")
print(subject_averages)

# 7. Save reports to CSV files in the working directory
write.csv(students, "student_performance_report.csv", row.names = FALSE)
write.csv(subject_averages, "subject_average_report.csv", row.names = FALSE)
cat("\\nCSV reports saved in your R working directory.\\n")

# 8. Bar chart: average marks of each student
barplot(students$Average,
        names.arg = students$Name,
        col = "skyblue",
        main = "Average Marks of Students",
        xlab = "Student",
        ylab = "Average Marks",
        ylim = c(0, 100),
        las = 2)
abline(h = 40, col = "red", lty = 2)
legend("bottomleft", legend = "Pass mark (40)", col = "red",
       lty = 2, bty = "n")

# 9. Bar chart: average marks by subject
barplot(subject_averages$AverageMarks,
        names.arg = subject_averages$Subject,
        col = "lightgreen",
        main = "Subject-wise Average Marks",
        xlab = "Subject",
        ylab = "Average Marks",
        ylim = c(0, 100))

# 10. Pie chart: pass/fail count
result_counts <- table(students$Result)
pie(result_counts,
    labels = paste(names(result_counts), result_counts),
    col = c("lightgreen", "tomato"),
    main = "Pass and Fail Distribution")

# 11. Scatter plot: attendance vs average marks
plot(students$Attendance, students$Average,
     pch = 19, col = "blue",
     main = "Attendance vs Average Marks",
     xlab = "Attendance (%)",
     ylab = "Average Marks",
     xlim = c(0, 100), ylim = c(0, 100))
text(students$Attendance, students$Average,
     labels = students$Name, pos = 3, cex = 0.7)

# 12. Optional: install and use ggplot2 for a polished chart.
# Uncomment the next two lines if ggplot2 is not installed.
# install.packages("ggplot2")
# library(ggplot2)
# ggplot(students, aes(x = Name, y = Average)) +
#   geom_col(fill = "steelblue") +
#   labs(title = "Student Average Marks", x = "Student", y = "Average Marks") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
