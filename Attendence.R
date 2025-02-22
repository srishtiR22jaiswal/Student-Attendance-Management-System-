# Load required packages
library(ggplot2)
library(dplyr)

# Initialize an empty attendance data frame
attendance_data <- data.frame(
  Name = character(),
  AttendanceDate = as.Date(character()),
  Status = logical(),
  stringsAsFactors = FALSE
)

# Function to mark attendance
mark_attendance <- function(name, status) {
  date <- Sys.Date()  # Get the current date
  attendance_data <<- rbind(attendance_data, 
                            data.frame(Name = name, AttendanceDate = date, Status = status))
  cat("✅ Attendance marked for", name, "on", Sys.Date(), "\n")
}

# Function to display overall attendance statistics
display_statistics <- function() {
  if (nrow(attendance_data) == 0) {
    cat("⚠ No attendance data available.\n")
    return()
  }
  
  total_students <- length(unique(attendance_data$Name))
  total_present <- sum(attendance_data$Status)
  total_absent <- nrow(attendance_data) - total_present
  
  cat("\n📊 Attendance Statistics 📊\n")
  cat("👥 Total Students:", total_students, "\n")
  cat("✅ Total Present:", total_present, "\n")
  cat("❌ Total Absent:", total_absent, "\n")
}

# Function to display individual attendance statistics
display_individual_statistics <- function() {
  if (nrow(attendance_data) == 0) {
    cat("⚠ No attendance data available.\n")
    return()
  }
  
  individual_stats <- attendance_data %>%
    group_by(Name) %>%
    summarise(
      Total_Present = sum(Status),
      Total_Absent = n() - Total_Present
    ) %>%
    arrange(desc(Total_Present))
  
  cat("\n📌 Individual Attendance Statistics 📌\n")
  print(individual_stats)
}

# Function to plot attendance as a pie chart
plot_attendance_pie <- function() {
  if (nrow(attendance_data) == 0) {
    cat("⚠ No attendance data available to plot.\n")
    return()
  }
  
  total_present <- sum(attendance_data$Status)
  total_absent <- nrow(attendance_data) - total_present
  slices <- c(total_present, total_absent)
  lbls <- c("✅ Present", "❌ Absent")
  
  # Create a pie chart
  pie(slices, labels = lbls, main = "📊 Attendance Summary", col = c("green", "red"))
}

# Main menu loop
repeat {
  cat("\n--- 📋 Attendance Management System ---\n")
  cat("1️⃣  Mark Attendance\n")
  cat("2️⃣  Display Overall Statistics\n")
  cat("3️⃣  Display Individual Statistics\n")
  cat("4️⃣  Plot Attendance Chart\n")
  cat("5️⃣  Exit\n")
  
  choice <- as.integer(readline("Enter your choice (1-5): "))
  
  if (choice == 1) {
    name <- readline("Enter student name: ")
    status <- as.integer(readline("Mark attendance (1 for Present, 0 for Absent): "))
    mark_attendance(name, as.logical(status))
    
  } else if (choice == 2) {
    display_statistics()
    
  } else if (choice == 3) {
    display_individual_statistics()
    
  } else if (choice == 4) {
    plot_attendance_pie()
    
  } else if (choice == 5) {
    cat("✅ Exiting the system. Have a great day!\n")
    break
    
  } else {
    cat("❌ Invalid choice. Please try again.\n")
  }
}
