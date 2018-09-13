  library(plyr)
  library(dplyr)
  library(stringr)
  library(RColorBrewer)
  library(RODBC)
  library(gdata)
  library(scales)
  library(data.table)

  
  uid = "ommitted to post on github"
  pwd = "ommitted to post on github"
  table<-NULL
    
  
  ethnicity_gender_levels = c("Asian Male", "Asian Female", 
                              "Black/African American Male", "Black/African American Female",
                              "Hawaiian/Pacific Islander Male", "Hawaiian/Pacific Islander Female",
                              "Hispanic/Latino Male", "Hispanic/Latino Female", 
                              "Native American Male", "Native American Female",
                              "Two or more races Male", "Two or more races Female",
                              "Unknown Male", "Unknown Female",
                              "White Male", "White Female")
  
  ethnicity_gender_palette = c(brewer.pal(12, "Paired"),brewer.pal(9,"Greys")[c(4,6)],brewer.pal(9,"PuRd")[c(4,6)])
  
  
  award_cat = function(x)
  {
    switch(x,
           "CS" = "Skills Certificate",
           "C" = "Certificate of Achievement",
           "AA" = "Associate in Arts",
           "AS" = "Associate in Science",
           "AT" = "Associate in Arts for Transfer",
           "ST" = "Associate in Science for Transfer",
           "CN" = "Noncredit Certificate",
           "Unknown award type")
  }
  
  semester_cat = function(x)
  {
    ifelse(x == 0, "Winter",
           ifelse(x == 1, "Spring", 
                  ifelse(x == 2, "Summer", "Fall")))
  }
  
  getCurrentTerm = function()
  {
    library(RODBC)
    library(plyr)
    library(dplyr)
    
    connection = odbcConnect("OIEA_SERVER",uid, pwd)
    query = "select max(Year_Semester) as Year_Semester from terms 
    where getdate() >= Term_Start_Date
    and College = 'E'"
    result = sqlQuery(connection, query,stringsAsFactors = FALSE)
    
    odbcClose(connection)
    
    return(result$Year_Semester)
  }
  
  getAcademicYear = function(term)
  {
    ##Calculate the Academic year. Ex: 20152016
    acad.yr = ifelse(sapply(term, substr,5,5) %in% c(2,3), 
                     paste0(sapply(term, substr,1,4),as.integer(sapply(term, substr,1,4))+1),
                     paste0(as.integer(sapply(term, substr,1,4))-1,sapply(term, substr,1,4)))
    
    
    return(as.integer(acad.yr))
  }
  
  getAwardsByMajor = function(major_codes, mode = "all", colleges = 'E', years = 6)
  {
    #load the underlying data set
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    #format multiple items as vectors
    colleges = paste0("(",paste0("'",colleges,"'"),")", collapse = ",")
    major_codes = paste0("(",paste0("'",major_codes,"'",collapse = ","),")")
    
    if(mode == "all")
    {
      #Go back 6 academic years
      academicYearCutoff = getAcademicYear(getCurrentTerm() - years*10)
      yearSemesterCutoff = as.integer(paste0(substr(academicYearCutoff,1,4),2))
      
      #Generate a list of academic years for use later in setting the levels of a factor
      academicYearLevels = formatAcademicYear(seq(academicYearCutoff, getAcademicYear(getCurrentTerm()), 10001))
      
      #generate a list of year semesters
      yearSemesterLevels = seq(yearSemesterCutoff, getCurrentTerm(),1)
      yearSemesterLevels = formatYearSemester(yearSemesterLevels[substr(yearSemesterLevels,5,5) %in% c(0,1,2,3)])
      
      query = paste0("declare @year_semester_cutoff int = ",yearSemesterCutoff,"
                     declare @current_term int = ",getCurrentTerm(),"
                     
                     
                     select College, Major_Code, Award_Type, Year_Semester, Gender, Ethnicity, Cohort, SGEC_Units, Online_Units
                     from 
                     (
                     select awards.Student_Id, awards.College, awards.Major_Code, awards.Award_Type, Award_Date, 
                     max(terms.Year_Semester) as Year_Semester, sum(SGEC_Units)/sum(Units_Attempted) as SGEC_Units, sum(Online_Units)/sum(Units_Attempted) as Online_Units,
                     Gender, Ethnicity
                     from awards 
                     left join students
                     on awards.Student_Id = students.Student_Id
                     left join programs 
                     on awards.College = programs.College and
                     awards.Award_Type = programs.Award_Type and
                     awards.Major_Code = programs.Major_Code and
                     awards.Catalog_Year = programs.Catalog_Year
                     left join terms
                     on Award_Date >= Term_Start_Date
                     left join student_profiles
                     on awards.Student_Id = student_profiles.Student_Id
                     and terms.Year_Semester = student_profiles.Year_Semester
                     and awards.College = student_profiles.College
                     where awards.College in ",colleges," 
                     and awards.Major_Code in ",major_codes," group by 
                     awards.Student_Id, awards.College, awards.Major_Code, awards.Award_Type, 
                     Award_Date, Gender, Ethnicity 
                     ) sub1 left join cohorts on sub1.Student_Id = cohorts.Student_Id
                     where Year_Semester >= @year_semester_cutoff and Year_Semester <= @current_term 
                     ")
    }
    else if(mode == "cohort")
    {
      #Go back 6 academic years - use July 1 as the cutoff
      yearSemesterCutoff = getCurrentTerm() - years*10
      
      academicYearCutoff = getAcademicYear(yearSemesterCutoff)
      
      #Generate a list of academic years for use later in setting the levels of a factor
      academicYearLevels = formatAcademicYear(seq(academicYearCutoff, getAcademicYear(getCurrentTerm()), 10001))
      
      #generate a list of year semesters
      yearSemesterLevels = seq(yearSemesterCutoff, getCurrentTerm(),1)
      yearSemesterLevels = formatYearSemester(yearSemesterLevels[substr(yearSemesterLevels,5,5) %in% c(0,1,2,3)])
      
      query = paste0("declare @cohort_cutoff int = ",yearSemesterCutoff,"
                     declare @current_term int = ",getCurrentTerm(),"
                     
                     select Cohort, College, Major_Code, Award_Type, Year_Semester, Gender, Ethnicity,
                     SGEC_Units, Online_Units
                     from
                     (
                     select awards.Student_Id, Cohort, awards.College, awards.Major_Code, Award_Type,
                     Award_Date, max(terms.Year_Semester) as Year_Semester, Gender, Ethnicity,
                     sum(SGEC_Units)/sum(Units_Attempted) as SGEC_Units, sum(Online_Units)/sum(Units_Attempted) as Online_Units
                     from
                     awards
                     left join 
                     cohorts on awards.Student_Id = cohorts.Student_Id
                     left join students on awards.Student_Id = students.Student_Id
                     left join terms on Award_Date >= Term_Start_Date
                     left join student_profiles
                     on awards.Student_Id = student_profiles.Student_Id
                     and terms.Year_Semester = student_profiles.Year_Semester
                     and awards.College = student_profiles.College
                     where Cohort_College = awards.College 
                     and Cohort_College in ",colleges," and awards.Major_Code in ",major_codes," group by 
                     awards.Student_Id, Cohort, awards.College, awards.Major_Code, Award_Type, Award_Date,
                     Gender, Ethnicity
                     ) sub1 where Cohort >= @cohort_cutoff and Cohort < @current_term 
                     ")
    }
    
    awards = sqlQuery(connection, query, stringsAsFactors = TRUE)
    odbcClose(connection)
    if(is.null(awards)|| nrow(awards)==0)
      return (NULL)
    
    awards = awards %>% arrange(Year_Semester)
    awards$Academic_Year = getAcademicYear(awards$Year_Semester)
    
    #format gender
    awards$Gender = as.character(awards$Gender)
    awards$Gender = ifelse(awards$Gender == "M", "Male", "Female")
    awards$Gender = factor(awards$Gender, levels = c("Male","Female"))
    
    #manually set the levels for ethnicity so that rarely occuring values aren't dropped
    awards$Ethnicity = as.character(awards$Ethnicity)
    awards$Ethnicity = factor(awards$Ethnicity,c("Asian", "Black/African American", "Hawaiian/Pacific Islander",
                                                 "Hispanic/Latino", "Native American","Two or more races",
                                                 "Unknown", "White"))
    
    #Add a terms to completion variable
    awards$Terms_to_Complete = getDifferentBetweenTerms(awards$Year_Semester,awards$Cohort)
    
    #format the academic year
    if(nrow(awards) > 0)
    {
      awards$Academic_Year = factor(formatAcademicYear(awards$Academic_Year),levels=academicYearLevels)
    }
    #format the year semester
    awards$Year_Semester = factor(formatYearSemester(awards$Year_Semester), levels = yearSemesterLevels)
    
    #format the award types
    awards$Award_Type = factor(sapply(as.character(awards$Award_Type), award_cat))
    
    return(awards)
    }
  
  formatYearSemester = function(x)
  {
    paste0(semester_cat(substr(x,5,5))," ",substr(x,1,4)) 
  }
  
  formatAcademicYear = function(x)
  {
    paste0(substr(x, 1,4),"-",substr(x, 7,8))  
  }
  
  getActivePrograms = function(colleges = 'E', years = 6)
  {
    #load the underlying data set
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    #Go back 6 academic years - use July 1 as the cutoff
    currentYear = getAcademicYear(getCurrentTerm()) - years*10000 - years
    cutoff = paste0(substr(currentYear,1,4),"-07-01")
    
    #format multiple items as vectors
    colleges = paste0("(",paste0("'",colleges,"'",collapse = ","),")")
    
    query = paste0("select Major_Code, Major
                   from
                   (
                   select Major_Code, Major,
                   ROW_NUMBER() over(partition by Major_Code order by Catalog_Year desc) as row
                   from programs
                   where College in ",colleges," and Catalog_Year >= ",currentYear," and
                   Catalog_Year <= ",getAcademicYear(getCurrentTerm())," and 
                   (Final_Term >= ",getCurrentTerm()," or Final_Term is null)
                   ) sub1 where row = 1")
    
    majors = sqlQuery(connection, query, stringsAsFactors = FALSE)
    odbcClose(connection)
    
    majors$Major_Code = factor(str_pad(majors$Major_Code, 6, "left","0"))
    majors$Major = factor(majors$Major)
    
    return(majors)
  }
  
  getDifferentBetweenTerms = function(y1, y2)
  {
    (as.integer(substr(y1,1,4)) - as.integer(substr(y2,1,4)))*4 + 
      (as.integer(substr(y1,5,5)) - as.integer(substr(y2,5,5)))
  }
  
  insertMissingFactorLevels = function(df, key, target, value = 0)
  {
    df = data.frame(df)
    
    #build a data frame of all possible factor level combos
    if(length(key) == 1)
    {
      expandedDf = expand.grid(levels(df[,key]))
      names(expandedDf) = key
    }
    else
    {
      expandedDf = expand.grid(sapply(df[,key],levels))
    }
    expandedDf = data.frame(expandedDf)
    
    expandedDf[target] = rep(value, nrow(expandedDf))
    
    storage.mode(expandedDf[,target]) = storage.mode(df[,target])
    
    #do an anti join to figure out which values are actually missing 
    #start by making a named vector for the join
    names(key) = key
    
    missingRows = expandedDf %>% anti_join(df, by = key)
    
    #append the rows that were missing
    df = rbind(df,missingRows)
    return(df)
  }
  
  interval_10_breaks = function(x)
  {
    seq(0,ceiling(max(x)*1.1),ceiling(max(x)*1.1/10))
  }
  
  interval_proportion_breaks = function(x)
  {
    seq(0,1,0.05)
  }
  
  generateAwardsPlot = function(rawData,dataSetType, tabCompleters, timeScale, plotStyle, 
                                genderChoice, ethnicityChoice, SGECUnits, onlineUnits, chartStyle, awardType, text=NULL)
  {
  
    if(is.null(rawData)){
      return(NULL)
    }
    rawData = rawData %>% filter(Award_Type %in% awardType)
    
    if(nrow(rawData) == 0)
    {
      return(grob(NULL))
    }
    
    if(tabCompleters %in% c("Completers","Cumulative Completers"))
    {
      #determine the time scale
      
      if(timeScale == "academic_year")
      {
        x_axis = "Academic_Year"
      }
      else
      {
        x_axis = "Year_Semester"
      }
      
      if(chartStyle == "count")
      {
        position = "stack"
      }
      else
      {
        position = "fill"
      }
      
      
      awards_data = rawData 
      if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
      {
        awards_data = awards_data %>% 
          group_by_(.dots = c(as.name(x_axis),as.name("Award_Type"))) %>% summarize(Count = n())
        
        if(tabCompleters == "Cumulative Completers")
        {
          awards_data = awards_data %>% arrange_(.dots = as.name(x_axis)) %>%
            group_by(Award_Type) %>% mutate(Count = cumsum(Count)) 
        }
        
        awards_plot = ggplot(data = awards_data, aes(x=get(x_axis), y = Count, group= Award_Type)) + 
          geom_bar(fill="goldenrod2",color = "black",stat="identity",position = position)
      }  
      else if(length(genderChoice) > 0 & length(ethnicityChoice) == 0) 
      {
        awards_data = awards_data %>% filter(Gender %in% genderChoice) %>%
          group_by_(.dots = c(as.name(x_axis),as.name("Award_Type"),as.name("Gender"))) %>% summarize(Count = n())
        
        if(tabCompleters == "Cumulative Completers")
        {
          awards_data = awards_data %>% arrange_(.dots = as.name(x_axis)) %>%
            group_by(Award_Type, Gender) %>% mutate(Count = cumsum(Count))
        }
        
        awards_plot = ggplot(data = awards_data, aes(x=get(x_axis), y = Count, fill=Gender, group = Gender)) +
          geom_bar(stat = "identity", position = position, color = "black") + 
          scale_fill_manual(values = c("darkgreen","goldenrod"),drop=FALSE)
      }
      else if(length(genderChoice) == 0 & length(ethnicityChoice) > 0) 
      {
        awards_data = awards_data %>% filter(Ethnicity %in% ethnicityChoice) %>%
          group_by_(.dots = c(as.name(x_axis),as.name("Award_Type"),as.name("Ethnicity"))) %>% summarize(Count = n())
        
        if(tabCompleters == "Cumulative Completers")
        {
          awards_data = awards_data %>% arrange_(.dots = as.name(x_axis)) %>%
            group_by(Award_Type,Ethnicity) %>% mutate(Count = cumsum(Count)) 
        }
        
        awards_plot = ggplot(data = awards_data, aes(x=get(x_axis), y = Count, fill=Ethnicity, group = Ethnicity)) +
          geom_bar(stat = "identity", position = position, color = "black") + 
          scale_fill_brewer(palette = "Dark2",drop=FALSE)
      }
      else 
      {
        awards_data = awards_data %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
          mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) %>%
          group_by_(.dots = c(as.name(x_axis),as.name("Award_Type"),as.name("Ethnicity_Gender"))) %>% summarize(Count = n())
        
        awards_data$Ethnicity_Gender = factor(awards_data$Ethnicity_Gender,levels = ethnicity_gender_levels)
        
        if(tabCompleters == "Cumulative Completers")
        {
          awards_data = awards_data %>% arrange_(.dots = as.name(x_axis)) %>%
            group_by(Award_Type,Ethnicity_Gender) %>% mutate(Count = cumsum(Count)) 
        }
        
        awards_plot = ggplot(data = awards_data, aes(x=get(x_axis), y = Count, fill=Ethnicity_Gender, group = Ethnicity_Gender)) +
          geom_bar(stat = "identity", position = position, color = "black") + 
          scale_fill_manual(values = ethnicity_gender_palette, drop = FALSE)
      }
      
      #For year semester time scale, place x-axis labels vertically
      if(timeScale == "semester")
      {
        awards_plot = awards_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
      }
      
      if(chartStyle == "count")
      {
        awards_plot = awards_plot + scale_y_continuous(breaks = interval_10_breaks)
      }
      else
      {
        awards_plot = awards_plot + scale_y_continuous(breaks = interval_proportion_breaks)
      }
      
      awards_plot = awards_plot +
        theme(axis.title.x = element_blank()) + 
        facet_wrap(~Award_Type,ncol=2, scales= "free_y") + 
        theme(axis.title.y = element_blank()) + scale_x_discrete(drop = FALSE) +
        ggtitle(dataSetType)
      
      
    }
    else if(tabCompleters == "Time to Complete Award")
    {
      awards_data = na.omit(rawData)
      
      if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
      {
        awards_plot = ggplot(awards_data, aes(x=Terms_to_Complete/4))
      } else if(length(genderChoice) > 0 & length(ethnicityChoice) == 0)
      {
        awards_data = awards_data %>% filter(Gender %in% genderChoice)
        awards_plot = ggplot(awards_data, aes(x=Terms_to_Complete/4,fill=Gender)) + 
          scale_fill_manual(values = c("darkgreen","darkblue"),drop=FALSE)
      } else if(length(genderChoice) == 0 & length(ethnicityChoice) >= 0)
      {
        awards_data = awards_data %>% filter(Ethnicity %in% ethnicityChoice)
        awards_plot = ggplot(awards_data, aes(x=Terms_to_Complete/4,fill=Ethnicity)) + 
          scale_fill_brewer(palette = "Dark2",drop=FALSE)
      } else 
      {
        #create a concatenated ethnicty/gender variable 
        awards_data = awards_data %>% filter(Gender %in% genderChoice, Ethnicity %in% ethnicityChoice) %>%
          mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender))
        
        awards_data$Ethnicity_Gender = factor(awards_data$Ethnicity_Gender, levels = ethnicity_gender_levels)
        
        awards_plot = ggplot(awards_data, aes(x=Terms_to_Complete/4,fill=Ethnicity_Gender)) + 
          scale_fill_manual(values = ethnicity_gender_palette, drop = FALSE)
      }
      
      if(nrow(awards_data)==0){
        return(NULL)
      }
      
      max_range = range(awards_data$Terms_to_Complete)[2]/4
      
      binwidth = ifelse(max_range > 10, 1, 0.5)
      
      if(max_range > 10) 
      {
        x_breaks = seq(0,max_range+1,1)
      } else { 
        x_breaks = seq(0,max_range+1,0.5)
      }
      
      if(plotStyle == "histogram")
      {
        if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
        {
          awards_plot = awards_plot +
            geom_histogram(binwidth = binwidth, color = "black", fill = "goldenrod")
        }
        else
        {
          awards_plot = awards_plot +
            geom_histogram(binwidth = binwidth, color = "black") 
        }
      }
      else
      {
        if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
        {
          awards_plot = awards_plot +
            geom_density(alpha = 0.4, fill = "goldenrod") 
        }
        else
        {
          awards_plot = awards_plot +
            geom_density(alpha = 0.4) 
        }
      }
      
      
      awards_plot = awards_plot + 
        xlab("Years to Completion") +
        facet_wrap(~Award_Type,ncol=2,scales = "free_y") +
        scale_x_continuous(breaks = x_breaks)  + ggtitle(dataSetType)
      
    }
    else if(tabCompleters %in% c("Completers (SGEC Students)","Completers (Online Students)"))
    {
      if(timeScale == "academic_year")
      {
        x_axis = "Academic_Year"
      }
      else
      {
        x_axis = "Year_Semester"
      }
      
      if(chartStyle == "count")
      {
        position = "stack"
      }
      else
      {
        position = "fill"
      }
      
      if(tabCompleters == "Completers (SGEC Students)")
      {
        status_type = "SGEC_Status"
        awards_data = rawData[!is.na(rawData$SGEC_Units),]
        awards_data$SGEC_Status = ifelse(awards_data$SGEC_Units*100 >= SGECUnits, "SGEC student", "Not an SGEC student")
        status_levels = c("Not an SGEC student","SGEC student")
      }
      else
      {
        status_type = "Online_Status"
        awards_data = rawData[!is.na(rawData$Online_Units),]
        awards_data$Online_Status = ifelse(awards_data$Online_Units*100 >= onlineUnits, "Online student", "Not an online student")
        status_levels = c("Not an online student","Online student")
      }
      
      awards_data = awards_data %>%
        group_by_(.dots = c(as.name(x_axis), as.name("Award_Type"), as.name(status_type))) %>% summarize(Count = n())
      awards_data = data.frame(awards_data)
      awards_data[,status_type] = factor(awards_data[,status_type],levels = status_levels)
      
      #awards_data = insertMissingFactorLevels(awards_data, c("Award_Type",x_axis,status_type),"Count")
      
      awards_plot = ggplot(awards_data,aes(x=get(x_axis), y=Count, fill=get(status_type),group=get(status_type))) +
        geom_bar(stat = "identity", color = "black", position = position) + facet_wrap(~Award_Type,ncol=2) + 
        scale_x_discrete(drop=FALSE) + scale_fill_manual(values = c("darkgreen","goldenrod"), drop=FALSE,name = "") +
        theme(axis.title.x = element_blank()) + 
        theme(axis.title.y = element_blank())  
      
      #For year semester time scale, place x-axis labels vertically
      if(timeScale == "semester")
      {
        awards_plot = awards_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())
      }
      
      if(chartStyle == "count")
      {
        awards_plot = awards_plot + scale_y_continuous(breaks = interval_10_breaks)
      }
      else
      {
        awards_plot = awards_plot + scale_y_continuous(breaks = interval_proportion_breaks)
      }
      
    }
    
    return(awards_plot$data)
  }
  
  getCoursePatterns = function(major_codes,colleges = 'E', years = 6, successCutoff = 0.75, failureCutoff = 0.5, minStudents = 5,
                               confidence = 0.8)
  {
    library(arules)
    library(arulesViz)
    library(plyr)
    library(dplyr)
    library(RODBC)
    
    #format multiple items as vectors
    colleges = paste0("(",paste0("'",colleges,"'"),")", collapse = ",")
    major_codes = paste0("(",paste0("'",major_codes,"'"),")", collapse = ",")
    
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    query = paste0("declare @cohort_cutoff int = ",getCurrentTerm() - 10*years,"
                   declare @current_term int = ",getCurrentTerm(),"
                   
                   select awards.Student_Id,enrollments.Year_Semester, Course, Completion
                   from
                   awards
                   left join 
                   cohorts on awards.Student_Id = cohorts.Student_Id
                   left join terms on Award_Date >= Term_Start_Date
                   left join enrollments
                   on awards.Student_Id = enrollments.Student_Id
                   left join sections2
                   on enrollments.Section_Number = sections2.Section_Number
                   and enrollments.College = sections2.College
                   and enrollments.Year_Semester = sections2.Year_Semester
                   left join 
                   (
                   select Student_Id, Year_Semester,sum(Units_Completed)/sum(Units_Attempted) as Completion
                   from
                   student_profiles 
                   group by Student_Id, Year_Semester
                   ) sub1
                   on awards.Student_Id = sub1.Student_Id
                   and enrollments.Year_Semester = sub1.Year_Semester
                   where Cohort_College = awards.College 
                   and Cohort_College in ",colleges," and awards.Major_Code in ",major_codes,"
                   and Grade != 'E' and sections2.Noncredit = 0
                   and Cohort >= @cohort_cutoff and Cohort <= @current_term 
                   group by awards.Student_Id, enrollments.Year_Semester, Course, Completion
                   ")
    
    courses_raw = sqlQuery(connection, query, stringsAsFactors = TRUE)
    odbcClose(connection)
    
    courses = courses_raw %>% filter(Completion >= successCutoff)
    
    courses$Id = as.integer(factor(paste0(courses$Student_Id,courses$Year_Semester)))
    
    courses = split(courses$Course, courses$Id)
    
    courses_trans = as(courses,"transactions")
    
    support = min(minStudents/length(courses),.3)
    
    rules = apriori(courses_trans, parameter = list(supp = support, conf = confidence), control = list(verbose=FALSE))
    redundant = is.redundant(rules)
    rules = rules[!redundant]
    
    successfulAttemptPatterns = rules
    
    courses = courses_raw %>% filter(Completion <= failureCutoff)
    
    courses$Id = as.integer(factor(paste0(courses$Student_Id,courses$Year_Semester)))
    
    courses = split(courses$Course, courses$Id)
    
    courses_trans = as(courses,"transactions")
    
    support = min(minStudents/length(courses),0.3)
    rules = apriori(courses_trans, parameter = list(supp = support, conf = confidence), control = list(verbose=FALSE))
    
    redundant = is.redundant(rules)
    rules = rules[!redundant]
    
    unsuccessfulAttemptPatterns = rules
    
    courses = courses_raw 
    
    courses$Id = as.integer(factor(paste0(courses$Student_Id,courses$Year_Semester)))
    
    courses = split(courses$Course, courses$Id)
    
    courses_trans = as(courses,"transactions")
    
    targetSupport = min(minStudents/length(courses),.3)
    
    rules = apriori(courses_trans, parameter = list(supp = targetSupport, conf = confidence), control = list(verbose=FALSE))
    redundant = is.redundant(rules)
    rules = rules[!redundant]
    
    allCoursePatterns = rules
    
    allCoursePatterns = as(allCoursePatterns,"data.frame")
    colnames(allCoursePatterns)[colnames(allCoursePatterns) == "rules"] = "Students who take this => take that"
    
    successfulAttemptPatterns = as(successfulAttemptPatterns,"data.frame")
    unsuccessfulAttemptPatterns = as(unsuccessfulAttemptPatterns,"data.frame")
    
    return(list(allCoursePatterns = allCoursePatterns,
                successfulAttemptPatterns = successfulAttemptPatterns, 
                unsuccessfulAttemptPatterns = unsuccessfulAttemptPatterns))
  }
  
  getStratifiedCohorts = function(major_codes,mode = "completers", colleges = 'E', cohort_length = 6,look_back=7)
  {
    #load the underlying data set
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    #format multiple items as vectors
    colleges = paste0("(",paste0("'",colleges,"'"),")", collapse = ",")
    major_codes = paste0("(",paste0("'",major_codes,"'",collapse = ","),")")
    
    #Go back 6 academic years - use July 1 as the cutoff
    yearSemesterCutoff = getCurrentTerm() - cohort_length*10 
    
    #generate a list of year semesters
    yearSemesterLevels = seq(yearSemesterCutoff, getCurrentTerm(),1)
    yearSemesterLevels = formatYearSemester(yearSemesterLevels[substr(yearSemesterLevels,5,5) %in% c(0,1,2,3)])
    
    if(mode == "completers")
    {
      query = paste0("declare @cohort_cutoff int = ",yearSemesterCutoff,"
                     declare @current_term int = ",getCurrentTerm(),"
                     declare @cohort_length int = ",cohort_length,"
                     declare @look_back_period int = ",look_back,";
                     
                     WITH gen AS (
                     SELECT @current_term-1 AS num
                     UNION ALL
                     SELECT num-1 FROM gen WHERE num-1 >= @cohort_cutoff - @cohort_length*10 - @look_back_period*10
                     ),
                     cohort_years as
                     (
                     SELECT cohort_terms.num as Cohort,award_terms.num as Year_Semester, completion_cohort_terms.num as Completion_Cohort FROM gen cohort_terms
                     cross join gen completion_cohort_terms
                     cross join gen award_terms
                     where substring(cast(cohort_terms.num as char),5,1) in ('0','1','2','3')
                     and substring(cast(completion_cohort_terms.num as char),5,1) in ('0','1','2','3')
                     and substring(cast(award_terms.num as char),5,1) in ('0','1','2','3')
                     and cohort_terms.num > completion_cohort_terms.num - @cohort_length*10 and completion_cohort_terms.num >= @cohort_cutoff
                     and award_terms.num >= cohort_terms.num and award_terms.num < completion_cohort_terms.num
                     ),
                     active_programs as
                     (
                     select College, Major_Code, Award_Type from
                     (
                     select College, Major_Code, Award_Type, max(Final_Term) as Final_Term from programs where Major_Code in ",major_codes," and College in ",colleges,"
                     group by College, Major_Code, Award_Type
                     ) sub1 where Final_Term is null
                     )
                     
                     select sub1.Cohort,cohort_years.Completion_Cohort, College, Major_Code, Award_Type, sub1.Year_Semester, Gender, Ethnicity
                     from
                     (
                     select awards.Student_Id, cohorts.Cohort, awards.College, awards.Major_Code, awards.Award_Type,
                     Award_Date, max(terms.Year_Semester) as Year_Semester, Gender, Ethnicity
                     from
                     awards
                     inner join active_programs
                     on awards.Major_Code = active_programs.Major_Code and
                     awards.College = active_programs.College and
                     awards.Award_Type = active_programs.Award_Type
                     left join 
                     cohorts on awards.Student_Id = cohorts.Student_Id
                     left join students on awards.Student_Id = students.Student_Id
                     left join terms on Award_Date >= Term_Start_Date
                     where Cohort_College = awards.College 
                     and Cohort_College in ",colleges," and awards.Major_Code in ",major_codes,"
                     and Cohort >= @cohort_cutoff - @look_back_period*10 and Cohort < @current_term 
                     group by 
                     awards.Student_Id, cohorts.Cohort, awards.College, awards.Major_Code, awards.Award_Type, Award_Date,
                     Gender, Ethnicity
                     ) sub1 
                     left join cohort_years
                     on sub1.Year_Semester = cohort_years.Year_Semester
                     and sub1.Cohort = cohort_years.Cohort
                     and sub1.Year_Semester < cohort_years.Completion_Cohort
                     option (maxrecursion 10000)			 		 
                     ")
    }
    else if(mode == "efficiency")
    {
      query = paste0("declare @cohort_cutoff int = ",yearSemesterCutoff,"
                     declare @current_term int = ",getCurrentTerm(),"
                     declare @cohort_length int = ",cohort_length,"
                     declare @look_back_period int = ",look_back,";
                     
                     WITH gen AS (
                     SELECT @current_term-1 AS num
                     UNION ALL
                     SELECT num-1 FROM gen WHERE num-1 >= @cohort_cutoff - @cohort_length*10 - @look_back_period*10
                     ),
                     cohort_years as
                     (
                     SELECT cohort_terms.num as Cohort,award_terms.num as Year_Semester, completion_cohort_terms.num as Completion_Cohort FROM gen cohort_terms
                     cross join gen completion_cohort_terms
                     cross join gen award_terms
                     where substring(cast(cohort_terms.num as char),5,1) in ('0','1','2','3')
                     and substring(cast(completion_cohort_terms.num as char),5,1) in ('0','1','2','3')
                     and substring(cast(award_terms.num as char),5,1) in ('0','1','2','3')
                     and cohort_terms.num > completion_cohort_terms.num - @cohort_length*10 and completion_cohort_terms.num >= @cohort_cutoff
                     and award_terms.num >= cohort_terms.num and award_terms.num < completion_cohort_terms.num
                     ),
                     active_programs as
                     (
                     select College, Major_Code, Award_Type from
                     (
                     select College, Major_Code, Award_Type, max(Final_Term) as Final_Term from programs where Major_Code in ",major_codes," and College in ",colleges,"
                     group by College, Major_Code, Award_Type
                     ) sub1 where Final_Term is null
                     )
                     
                     select cohort_years.Completion_Cohort, sub1.College, 
                     Major_Code, Award_Type, 
                     count(distinct sub1.Student_Id) as Completers,
                     sum(FTES/nullif(Enrollment,0))  as FTES,
                     sum((Standard_Hours/Teaching_Load)/nullif(Enrollment,0)) as FTEF,
                     sum(FTES/nullif(Enrollment,0)) / sum((Standard_Hours/Teaching_Load)/nullif(Enrollment,0)) as Efficiency
                     from
                     (
                     select awards.Student_Id, cohorts.Cohort, awards.College, awards.Major_Code, awards.Award_Type,
                     Award_Date, max(terms.Year_Semester) as Year_Semester
                     from
                     awards
                     inner join active_programs 
                     on awards.Major_Code = active_programs.Major_Code and
                     awards.College = active_programs.College and
                     awards.Award_Type = active_programs.Award_Type
                     left join 
                     cohorts on awards.Student_Id = cohorts.Student_Id
                     left join terms on Award_Date >= Term_Start_Date
                     where Cohort_College = awards.College 
                     and Cohort_College in ",colleges," and awards.Major_Code in ",major_codes,"
                     and Cohort >= @cohort_cutoff - @look_back_period*10 and Cohort < @current_term 
                     group by 
                     awards.Student_Id, cohorts.Cohort, awards.College, awards.Major_Code, awards.Award_Type, Award_Date
                     ) sub1 
                     left join cohort_years
                     on sub1.Year_Semester = cohort_years.Year_Semester
                     and sub1.Cohort = cohort_years.Cohort
                     and sub1.Year_Semester < cohort_years.Completion_Cohort
                     left join enrollments 
                     on sub1.Student_Id = enrollments.Student_Id
                     left join sections2
                     on enrollments.Section_Number = sections2.Section_Number
                     and enrollments.College = sections2.College
                     and enrollments.Year_Semester = sections2.Year_Semester
                     and sub1.College = sections2.College
                     where 
                     Grade != 'E' 
                     and enrollments.Year_Semester < @current_term
                     and enrollments.Year_Semester <= sub1.Year_Semester 
                     and enrollments.Year_Semester >= sub1.Cohort
                     group by cohort_years.Completion_Cohort, sub1.College, 
                     Major_Code, Award_Type
                     option (maxrecursion 10000)			 		 ")  
    }
    
    awards = sqlQuery(connection, query, stringsAsFactors = TRUE)
    odbcClose(connection)
    
    awards = na.omit(awards)
    
    if(mode == "completers")
    {
      awards = awards %>% arrange(Year_Semester)
      
      #format gender
      awards$Gender = as.character(awards$Gender)
      awards$Gender = ifelse(awards$Gender == "M", "Male", "Female")
      awards$Gender = factor(awards$Gender, levels = c("Male","Female"))
      
      #manually set the levels for ethnicity so that rarely occuring values aren't dropped
      awards$Ethnicity = as.character(awards$Ethnicity)
      awards$Ethnicity = factor(awards$Ethnicity,c("Asian", "Black/African American", "Hawaiian/Pacific Islander",
                                                   "Hispanic/Latino", "Native American","Two or more races",
                                                   "Unknown", "White"))
      
      #Add a terms to completion variable
      awards$Terms_to_Complete = getDifferentBetweenTerms(awards$Year_Semester,awards$Cohort)
      
      awards$Year_Semester = NULL
      awards$Cohort = NULL
    }
    
    #Create a term variable
    awards$Term = factor(semester_cat(substr(awards$Completion_Cohort,5,5)), levels = c("Summer","Fall","Winter","Spring"))
    
    #format the year semester
    awards$Completion_Cohort = factor(formatYearSemester(awards$Completion_Cohort), levels = yearSemesterLevels)
    
    #format the award types
    awards$Award_Type = factor(sapply(as.character(awards$Award_Type), award_cat))
    
    awards = data.frame(awards)
    
    return(awards)
    }
  
  generateChangePlot = function(rawData,tabCompleters,genderChoice, ethnicityChoice, chartStyle, termChoice, awardType)
  {
    rawData = rawData %>% filter(Term %in% termChoice, Award_Type %in% awardType)
    
    if(nrow(rawData) == 0)
    {
      return(grob(NULL))
    }
    #--------------------------------------Change in Completers over Time Tab -------------------------------------------------------
    if(tabCompleters == "Change in Completers over Time")
    {
      if(chartStyle == "count")
      {
        position = "stack"
      }
      else
      {
        position = "fill"
      }
      
      change_data = rawData
      if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
      {
        change_data = change_data %>% 
          group_by(Completion_Cohort,Award_Type) %>% summarize(Count = n())
        
        change_plot = ggplot(data = change_data, aes(x=Completion_Cohort, y = Count, group= Award_Type)) + #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<CHANGE
          geom_bar(fill="goldenrod2",stat="identity", position = position, color = "black")
      }  
      else if(length(genderChoice) > 0 & length(ethnicityChoice) == 0) 
      {
        change_data = change_data %>% filter(Gender %in% genderChoice) %>%
          group_by(Completion_Cohort, Award_Type, Gender) %>% summarize(Count = n())
        
        change_plot = ggplot(data = change_data, aes(x=Completion_Cohort, y = Count, fill=Gender, group = Gender)) + #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<CHANGE
          geom_bar(stat = "identity",position=position, color = "black") + 
          scale_fill_manual(values = c("darkgreen","goldenrod"),drop=FALSE)
      }
      else if(length(genderChoice) == 0 & length(ethnicityChoice) > 0) 
      {
        change_data = change_data %>% filter(Ethnicity %in% ethnicityChoice) %>%
          group_by(Completion_Cohort,Award_Type,Ethnicity) %>% summarize(Count = n())
        
        change_plot = ggplot(data = change_data, aes(x=Completion_Cohort, y = Count, fill=Ethnicity, group = Ethnicity)) + #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<CHANGE
          geom_bar(stat = "identity",position=position, color = "black") + 
          scale_fill_brewer(palette = "Dark2",drop=FALSE)
        
      }
      else 
      {

        change_data = change_data %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
          mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) %>%
          group_by(Completion_Cohort,Award_Type,Ethnicity_Gender) %>% summarize(Count = n())
        
        change_data$Ethnicity_Gender = factor(change_data$Ethnicity_Gender,levels = ethnicity_gender_levels)
        
        change_plot = ggplot(data = change_data, aes(x=Completion_Cohort, y = Count, fill=Ethnicity_Gender, group = Ethnicity_Gender)) + #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<CHANGE
          geom_bar(stat = "identity",position = position, color = "black") + 
          scale_fill_manual(values = ethnicity_gender_palette, drop=FALSE) 

      }
      
      if(chartStyle == "count")
      {
        change_plot = change_plot + scale_y_continuous(breaks = interval_10_breaks) 
      }
      else
      {
        change_plot = change_plot + scale_y_continuous(breaks = interval_proportion_breaks) 
      }
      
      change_plot = change_plot + 
        facet_wrap(~Award_Type,ncol=2, scales= "free_y")
      
    }
    #----------------------------------Change in Efficiency Over Time Tab-----------------------------------------------------------
    else if(tabCompleters == "Change in Efficiency over Time")
    {
      change_data = rawData
      
      limits = c(0,ceiling(max(change_data$Efficiency)*1.10))
      
      change_plot = ggplot(change_data, aes(x=Completion_Cohort,y=Efficiency, group=Award_Type, color = Award_Type)) +
        geom_line(size=0.75) + geom_point(shape = 21, fill = "white", size = 3) + scale_y_continuous(limits = limits)
    }
    #----------------------------------Change in Time to Complete Award Tab---------------------------------------------------------
    else if(tabCompleters == "Change in Time to Complete Award")
    {
      change_data = rawData
      
      if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
      {
        change_plot = ggplot(change_data, aes(x=Completion_Cohort, y = Terms_to_Complete/4)) + 
          geom_boxplot(fill = "goldenrod") 
      }
      else if(length(genderChoice) > 0 & length(ethnicityChoice) == 0)
      {
        change_data = change_data %>% filter(Gender %in% genderChoice)
        
        change_plot = ggplot(change_data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Gender)) + 
          geom_boxplot() + scale_fill_manual(values = c("darkgreen","goldenrod"),drop=FALSE)
      }
      else if(length(genderChoice) == 0 & length(ethnicityChoice) > 0)
      {
        change_data = change_data %>% filter(Ethnicity %in% ethnicityChoice)
        
        change_plot = ggplot(change_data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Ethnicity)) + 
          geom_boxplot() +  scale_fill_brewer(palette = "Dark2",drop=FALSE)
      }
      else
      {
        change_data = change_data %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
          mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) 
        change_plot = ggplot(change_data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Ethnicity_Gender)) + 
          geom_boxplot() + scale_fill_manual(values = ethnicity_gender_palette, drop=FALSE) 
      }
      
      change_plot = change_plot +
        facet_wrap(~Award_Type,ncol=2, scales= "free_y") +
        scale_y_continuous(breaks = interval_10_breaks)
    }
    
    change_plot = change_plot + 
      theme(axis.title.x = element_blank()) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank()) + 
      theme(axis.title.y = element_blank()) + scale_x_discrete(drop=TRUE)
    
    return(change_plot$data)
  }
  
  getCourseDistributions = function(major_codes,colleges = 'E', years = 6)
  {
    library(plyr)
    library(dplyr)
    library(RODBC)
    
    #format multiple items as vectors
    colleges = paste0("(",paste0("'",colleges,"'"),")", collapse = ",")
    major_codes = paste0("(",paste0("'",major_codes,"'",collapse = ","),")")
    
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    query = paste0("declare @cohort_cutoff int = ",getCurrentTerm() - 10*years,"
                   declare @current_term int = ",getCurrentTerm(),"
                   
                   select Discipline, Course,awards.College, Major_Code, Award_Type, Gender, Ethnicity
                   from
                   awards
                   left join 
                   cohorts on awards.Student_Id = cohorts.Student_Id
                   left join enrollments
                   on awards.Student_Id = enrollments.Student_Id
                   left join sections2
                   on enrollments.Section_Number = sections2.Section_Number
                   and enrollments.College = sections2.College
                   and enrollments.Year_Semester = sections2.Year_Semester
                   left join students
                   on awards.Student_Id = students.Student_Id
                   where Cohort_College = awards.College 
                   and Cohort_College in ",colleges," and awards.Major_Code in ",major_codes,"
                   and Grade != 'E'
                   and Cohort >= @cohort_cutoff and Cohort < @current_term 
                   and enrollments.Year_Semester >= Cohort and enrollments.Year_Semester < @current_term
                   group by awards.Student_Id, Discipline, Course,awards.College, Major_Code, Award_Type, Gender, Ethnicity
                   ")
    
    courses = sqlQuery(connection, query, stringsAsFactors = TRUE)
    odbcClose(connection)
    
    #make factors
    courses$Discipline = as.factor(courses$Discipline)
    courses$Course = as.factor(courses$Course)
    #format the award types
    courses$Award_Type = factor(sapply(as.character(courses$Award_Type), award_cat))
    
    #format gender
    courses$Gender = as.character(courses$Gender)
    courses$Gender = ifelse(courses$Gender == "M", "Male", "Female")
    courses$Gender = factor(courses$Gender, levels = c("Male","Female"))
    
    #manually set the levels for ethnicity so that rarely occuring values aren't dropped
    courses$Ethnicity = as.character(courses$Ethnicity)
    courses$Ethnicity = factor(courses$Ethnicity,c("Asian", "Black/African American", "Hawaiian/Pacific Islander",
                                                   "Hispanic/Latino", "Native American","Two or more races",
                                                   "Unknown", "White"))
    return(courses)
  }
  
  getTopProgramCourses = function(majorCode, awardType, college = 'E', years = 6, cohortEnd = getCurrentTerm(), countCutoff = 25, 
                                  specialCourses = c('MATH 105', 'MATH 110','MATH 112', 'MATH 115', 'MATH 125',
                                                     'ENGLISH 019', 'ENGLISH 021','ENGLISH 026', 'ENGLISH 028',
                                                     'READING 019', 'READING 020'))
  {
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    courseList = paste0("(",paste0("'",specialCourses,"'", collapse = ','),")")
    
    query = paste0("declare @cohort_cutoff int = ",cohortEnd - 10*years,"
                   declare @current_term int = ",cohortEnd,"
                   declare @major_code char(6) = '",majorCode,"'
                   declare @award_type varchar(2) = '",awardType,"'
                   declare @college char(1) = '",college,"'
                   declare @n int = ",countCutoff,";
                   
                   select Course from
                   (
                   select top (@n) Course, count(*) as Count
                   from
                   awards
                   left join 
                   cohorts on awards.Student_Id = cohorts.Student_Id
                   left join enrollments
                   on awards.Student_Id = enrollments.Student_Id
                   left join sections2
                   on enrollments.Section_Number = sections2.Section_Number
                   and enrollments.College = sections2.College
                   and enrollments.Year_Semester = sections2.Year_Semester
                   where Cohort_College = awards.College 
                   and Cohort_College = @college and awards.Major_Code = @major_code and Award_Type = @award_type
                   and Grade != 'E'
                   and Cohort >= @cohort_cutoff and Cohort < @current_term 
                   and enrollments.Year_Semester >= Cohort and enrollments.Year_Semester < @current_term
                   and Course not in ",courseList,"
                   group by Course order by Count desc
                   ) sub1")
    
    courses = sqlQuery(connection, query, stringsAsFactors = FALSE)
    odbcClose(connection)
    
    courses = courses$Course
    
    courses = c(courses,specialCourses)
    
    return(courses)
  }
  
  #---------------------------------------------Course-taking Patterns----------------------------------------------------------------
  generateCourseDistributionPlot = function(courses, students, awardType, minStudents = 3,proportionCutoff = 0.25,
                                            genderChoice = NULL, ethnicityChoice = NULL, graph= F)
  {
    courses = courses %>% filter(Award_Type == awardType)
    
    if(length(genderChoice) == 0 & length(ethnicityChoice) == 0)
    {
      students_data = students %>% group_by(College, Major_Code,Award_Type) %>% 
        summarize(Group_Size = n())
      
      course_data = courses %>% 
        group_by(College, Major_Code, Award_Type, Course) %>% summarize(Count = n()) %>%
        left_join(students_data, by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type")) %>% 
        mutate(Proportion = Count/Group_Size) 
      
      course_data$Proportion[is.nan(course_data$Proportion)] = 0
      
      course_list = course_data %>% group_by(College, Major_Code, Award_Type, Course) %>% 
        summarize(Proportion = max(Proportion)) %>%
        filter(Proportion > proportionCutoff) %>% distinct(Course)
      
      course_data = course_data %>% inner_join(course_list, 
                                               by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type","Course" = "Course"))
      
      course_data$Category = factor(rep("All Students",nrow(course_data)))
      
      g = ggplot(course_data,aes(x = Category, y= Course,fill=Proportion))+scale_fill_gradient(high = '#1D7DBB', low = '#700001')
    }
    else if(length(genderChoice) > 0 & length(ethnicityChoice) == 0)
    {
      students_data = students %>% filter(Gender %in% genderChoice) %>%
        group_by(College, Major_Code, Award_Type,Gender) %>% summarize(Group_Size = n())
      
      course_data = courses %>% filter(Gender %in% genderChoice) %>%
        group_by(College, Major_Code, Award_Type,Gender, Course) %>% summarize(Count = n()) 
      
      course_data = course_data %>% left_join(students_data,
                                              by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type","Gender" = "Gender")) %>% 
        group_by(College, Major_Code, Award_Type,Gender, Course) %>% mutate(Proportion = Count/Group_Size) 
      course_data$Proportion[is.nan(course_data$Proportion)] = 0
      
      course_list = course_data %>% group_by(College, Major_Code, Award_Type,Course) %>% summarize(Proportion = max(Proportion)) %>%
        filter(Proportion > proportionCutoff) %>% distinct(Course)
      
      course_data = course_data %>% inner_join(course_list,
                                               by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type","Course" = "Course")) %>% 
        filter(Group_Size >= minStudents)
      
      g = ggplot(course_data,aes(x = Gender, y= Course,fill=Proportion))
      
    }
    else if(length(genderChoice) == 0 & length(ethnicityChoice) > 0)
    {
      students_data = students %>% filter(Ethnicity %in% ethnicityChoice) %>%
        group_by(College, Major_Code, Award_Type,Ethnicity) %>% summarize(Group_Size = n())
      
      course_data = courses %>% filter(Ethnicity %in% ethnicityChoice) %>%
        group_by(College, Major_Code, Award_Type,Ethnicity, Course) %>% summarize(Count = n()) 
      
      course_data = course_data %>% left_join(students_data,
                                              by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type", "Ethnicity" = "Ethnicity")) %>% 
        group_by(College, Major_Code, Award_Type,Ethnicity, Course) %>% mutate(Proportion = Count/Group_Size) 
      course_data$Proportion[is.nan(course_data$Proportion)] = 0
      
      course_list = course_data %>% group_by(College, Major_Code, Award_Type,Course) %>% summarize(Proportion = max(Proportion)) %>%
        filter(Proportion > proportionCutoff) %>% distinct(Course)
      
      course_data = course_data %>% inner_join(course_list, 
                                               by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type", "Course" = "Course")) %>% 
        filter(Group_Size >= minStudents)
      
      g = ggplot(course_data,aes(x = Ethnicity, y= Course,fill=Proportion))
    }
    else
    {
      students_data = students %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
        mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) %>%
        group_by(College, Major_Code, Award_Type,Ethnicity_Gender) %>% summarize(Group_Size = n())
      
      course_data = courses %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
        mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) %>%
        group_by(College, Major_Code, Award_Type,Ethnicity_Gender, Course) %>% summarize(Count = n()) 
      
      course_data = course_data %>% left_join(students_data,
                                              by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type", "Ethnicity_Gender" = "Ethnicity_Gender")) %>% 
        group_by(College, Major_Code, Award_Type,Ethnicity_Gender, Course) %>% mutate(Proportion = Count/Group_Size) 
      course_data$Proportion[is.nan(course_data$Proportion)] = 0
      
      course_list = course_data %>% group_by(College, Major_Code, Award_Type,Course) %>% summarize(Proportion = max(Proportion)) %>%
        filter(Proportion > proportionCutoff) %>% distinct(Course)
      
      course_data = course_data %>% inner_join(course_list,
                                               by = c("College" = "College", "Major_Code" = "Major_Code", "Award_Type" = "Award_Type", "Course" = "Course")) %>% 
        filter(Group_Size >= minStudents)
      
      g = ggplot(course_data,aes(x = Ethnicity_Gender, y= Course,fill=Proportion))
    }
    #Categotry, Course,Proportion
    
    g = g + geom_raster() +
      scale_fill_gradient(high = '#1D7DBB', low = '#700001')+ 
      labs(y="", x = "Category")+theme(panel.background = element_blank(), panel.grid.major.x = element_blank(),axis.title.x = element_text(margin = margin(t = 20)))
    g_plotly<-ggplotly(g, tooltip=c("Category","Course","Proportion")) %>% layout(margin = list( b = 100, l = 200, pad = 4))
    
    if(graph)
      return(g_plotly)
    
    return(g$data)
  }
  
  getSurvivalCohort = function(majorCode, awardType,coursePattern,college = 'E', years = 6, cohortEnd = getCurrentTerm())
  {
    connection = odbcConnect("OIEA_SERVER", uid, pwd)
    
    cleanedCoursePattern = gsub(" ","_",coursePattern, fixed = TRUE)
    cleanedCoursePattern = gsub(".","_",cleanedCoursePattern, fixed = TRUE)
    
    #[MATH 110] as MATH_110, [MATH 115] as MATH_115
    aliasQuery = paste0("[",coursePattern,"] as ",cleanedCoursePattern, collapse = ",")
    
    #('MATH 110', 'MATH 115')
    courseListQuery = paste0("(",paste0("'",coursePattern,"'", collapse = ","),")")
    
    #([MATH 105],[MATH 110])
    pivotCourseListQuery = paste0("(",paste0("[",coursePattern,"]", collapse = ","),")")
    
    #isnull(MATH_105,'H') as MATH_105, isnull(MATH_110,'H') as MATH_110
    courseSelectQuery = paste0("isnull(",cleanedCoursePattern,",0) as ",cleanedCoursePattern, collapse = ",")
    
    query = paste0("declare @cohort_cutoff int = ",cohortEnd - years*10,"
                   declare @current_term int = ",cohortEnd,"
                   declare @major_code char(6) = '",majorCode,"'
                   declare @award_type varchar(2) = '",awardType,"'
                   declare @college char(1) = '",college,"';
                   
                   with award_group as
                   (
                   select awards.Student_Id, max(Year_Semester) as Award_Year_Semester, 1 as Earned_Award
                   from awards
                   left join terms
                   on Award_Date >= Term_Start_Date
                   where Major_Code = @major_code and Award_Type = @award_type and awards.College = @college
                   group by awards.Student_Id
                   ),
                   previous_psa_concurrent as
                   (
                   select cohorts.Student_Id, max(PSA_Only) as Former_PSA_Student, 
                   max(Concurrent_Student) as Former_Concurrent_Student
                   from
                   cohorts left join
                   student_profiles
                   on cohorts.Student_Id = student_profiles.Student_Id
                   and cohorts.Cohort_College = student_profiles.College
                   where
                   Cohort >= @cohort_cutoff and Cohort < @current_term and
                   student_profiles.Year_Semester < Cohort and Cohort_College = @college
                   group by cohorts.Student_Id
                   ),
                   first_term as
                   (
                   select cohorts.Student_Id, 
                   case when Major_Code = @major_code then 1 else 0 end as Declared_Major_at_Entry
                   from
                   cohorts left join
                   student_profiles
                   on cohorts.Student_Id = student_profiles.Student_Id
                   and cohorts.Cohort_College = student_profiles.College
                   where
                   Cohort >= @cohort_cutoff and Cohort < @current_term and
                   student_profiles.Year_Semester = Cohort and Cohort_College = @college
                   ),
                   numbers as
                   (
                   select 19741 as Year_Semester
                   union all 
                   select Year_Semester + 1
                   from numbers
                   where Year_Semester + 1 < @current_term 
                   ),
                   terms_list as
                   (
                   select * from numbers
                   where substring(cast(Year_Semester as char),5,1) in ('0','1','2','3')
                   ),
                   course_outcomes as
                   (
                   select Student_Id, Year_Semester, ",aliasQuery,"
                   from
                   (
                   select cohorts.Student_Id, terms_list.Year_Semester, Course, Grade
                   from
                   cohorts left join
                   enrollments 
                   on cohorts.Student_Id = enrollments.Student_Id
                   left join sections2
                   on enrollments.Year_Semester = sections2.Year_Semester
                   and enrollments.College = sections2.College
                   and enrollments.Section_Number = sections2.Section_Number
                   left join 
                   terms_list 
                   on enrollments.Year_Semester <= terms_list.Year_Semester
                   where
                   Cohort >= @cohort_cutoff and Cohort < @current_term and
                   enrollments.Year_Semester < @current_term and Cohort_College = @college
                   and Grade in ('A','B','C','P')
                   and Course in ",courseListQuery,"
                   ) sub1 
                   pivot (count(Grade) for Course in ",pivotCourseListQuery,") as Grade
                   )
                   
                   select sub1.Year_Semester, Previous_Year_Semester, Cohort,
                   Term_Age,
                   Transfer_Goal, Undecided_Goal, CSU_UC_Student, Degree_Goal, Career_Goal,
                   Military, AB_540_Student, California_Resident, International_Student,
                   DSPS, EOPS,
                   CalWorks, BOGG, Total_Award, Units_Completed, Declared_Major_at_Entry,
                   GPA, Online_Only_Terms, SGEC_Only_Terms, Former_Concurrent_Student,
                   Former_PSA_Student, Female, Foster_Youth, Spanish_Speaking, Chinese_Speaking, Vietnamese_Speaking,
                   ethnicity_details.*, ",courseSelectQuery,",
                   Earned_Award
                   from
                   (
                   select cohorts.Student_Id, Year_Semester, Cohort,
                   lag(Year_Semester,1, Cohort) over (partition by cohorts.Student_Id order by cohorts.Student_Id, Year_Semester) as Previous_Year_Semester,
                   Term_Age - 18 as Term_Age,
                   case when Education_Goal = 'Transfer' then 1 else 0 end as Transfer_Goal,
                   case when Education_Goal = 'Undecided' then 1 else 0 end as Undecided_Goal,
                   case when Education_Goal = 'UC/CSU Student' then 1 else 0 end as CSU_UC_Student,
                   case when Education_Goal = 'Associate''s Degree' then 1 else 0 end as Degree_Goal,
                   case when Education_Goal = 'Career/Job Advancement' then 1 else 0 end as Career_Goal,
                   Declared_Major_at_Entry,
                   Military, 
                   case when Residency_Status = 'AB 540' then 1 else 0 end as AB_540_Student,
                   case when Residency_Status = 'California Resident' then 1 else 0 end as California_Resident,
                   case when Residency_Status = 'International' then 1 else 0 end as International_Student,
                   DSPS,
                   sum(EOPS) over (partition by cohorts.Student_Id order by cohorts.Student_Id, Year_Semester) as EOPS,
                   sum(CalWorks) over (partition by cohorts.Student_Id order by cohorts.Student_Id, Year_Semester) as CalWorks, 
                   sum(BOGG) over (partition by cohorts.Student_Id order by cohorts.Student_Id, Year_Semester) as BOGG, 
                   avg(Total_Award/4) over (partition by cohorts.Student_Id order by cohorts.Student_Id, Year_Semester) as Total_Award, 
                   sum(Units_Completed) over (partition by cohorts.Student_Id order by Year_Semester) as Units_Completed, 
                   District_Cumulative_GPA - 2.0 as GPA, 
                   sum(Online_Only) over (partition by cohorts.Student_Id order by Year_Semester) as Online_Only_Terms, 
                   sum(SGEC_Only) over (partition by cohorts.Student_Id order by Year_Semester) as SGEC_Only_Terms, 
                   isnull(Former_Concurrent_Student,0) as Former_Concurrent_Student, 
                   isnull(Former_PSA_Student,0) as Former_PSA_Student,
                   case when Gender = 'F' then 1 else 0 end as Female, 
                   case when Language = 'Spanish' then 1 else 0 end as Spanish_Speaking, 
                   case when Language = 'Chinese' then 1 else 0 end as Chinese_Speaking,
                   case when Language = 'Vietnamese' then 1 else 0 end as Vietnamese_Speaking,
                   Foster_Youth, isnull(Earned_Award,0) as Earned_Award,
                   ROW_NUMBER() over (partition by cohorts.Student_Id,isnull(Earned_Award,0) order by Year_Semester, isnull(Earned_Award,0)) as rk
                   from
                   cohorts 
                   left join
                   students 
                   on cohorts.Student_Id = students.Student_Id
                   left join
                   student_profiles
                   on cohorts.Student_Id = student_profiles.Student_Id
                   and cohorts.Cohort_College = student_profiles.College
                   left join award_group
                   on cohorts.Student_Id = award_group.Student_Id
                   and student_profiles.Year_Semester >= Award_Year_Semester
                   left join previous_psa_concurrent
                   on cohorts.Student_Id = previous_psa_concurrent.Student_Id
                   left join first_term
                   on cohorts.Student_Id = first_term.Student_Id
                   where
                   Cohort >= @cohort_cutoff and Cohort < @current_term
                   and student_profiles.Year_Semester > Cohort and student_profiles.Year_Semester < @current_term
                   and Cohort_College = @college
                   ) sub1 
                   left join ethnicity_details
                   on sub1.Student_Id = ethnicity_details.Student_Id
                   left join course_outcomes
                   on sub1.Student_Id = course_outcomes.Student_Id
                   and sub1.Year_Semester = course_outcomes.Year_Semester
                   where ((Earned_Award = 1 and rk = 1) or Earned_Award = 0)
                   order by sub1.Student_Id, sub1.Year_Semester
                   OPTION (MAXRECURSION 10000)")
    
    students = sqlQuery(connection,query, stringsAsFactors = TRUE)
    
    odbcClose(connection)
    
    
    students$Year_Semester = getDifferentBetweenTerms(students$Year_Semester,students$Cohort)
    students$Previous_Year_Semester = getDifferentBetweenTerms(students$Previous_Year_Semester,students$Cohort)
    
    return(students)
  }
  
  createEthnicityGenderCombos = function(ethnicity, gender)
  {
    
  }
  
  #will get a column and split it into varius columns 
  #Ex)instead of column names being Academic_Year, Award_Type, Gender, Count
  #     ->will result in   Academic_Year, Award_Type, Female, Female_Proportions,Male, Male_Proportions  (Note: count will be omitted and count number will be in the associated gender column)
  adjust_data_table<-function(data,catagories_column,time_scale){
     #proportions are included in ther dataframe data
   
  
     #get new column names
    data %>% mutate_if(is.factor, as.character) -> data

    column_names<-unique(data[[catagories_column]])
    prop=paste0(column_names," Proportion")
   
    award_types<-unique(data$Award_Type)
 
    #for the new data frame
    
    if(time_scale=="academic_year"){
      time_scale="Academic_Year"
      years<-as.character(levels(data$Academic_Year))
      result<-data.frame(Academic_Year=years,Award_Type=award_types[1])
    }
    else{  
      time_scale="Year_Semester"
      years<-as.character(levels(data$Year_Semester))
      result<-data.frame(Year_Semester=years,Award_Type=award_types[1])

    }

    #only if multiple award_types
    if(length(award_types)>1){
      for(i in 2:length(award_types)){
        if(time_scale=="academic_year")
          result=rbind(result,data.frame(Academic_Year=years,Award_Type=award_types[i]))
        else
          result=rbind(result,data.frame(Year_Semester=years,Award_Type=award_types[i]))
      }
    }
table<<-data

    #match the count number with the new column names
    for(i in 1:length(column_names)){
      result[[column_names[i]]]=0
      result[[prop[i]]]=0
     
      for(j in 1:nrow(result)){
         if(nrow(data[data[[time_scale]]==result[[time_scale]][j] & data$Award_Type==result$Award_Type[j]
                    & data[[catagories_column]]==column_names[i],])!=0){
            result[[column_names[i]]][j]=data[data[[time_scale]]==result[[time_scale]][j] & data$Award_Type==result$Award_Type[j]
                                             & data[[catagories_column]]==column_names[i],]$Count}
        if(nrow(data[data[[time_scale]]==result[[time_scale]][j] & data$Award_Type==result$Award_Type[j]
                     & data[[catagories_column]]==column_names[i],])!=0){
            result[[prop[i]]][j]=data[data[[time_scale]]==result[[time_scale]][j] & data$Award_Type==result$Award_Type[j]
                                      & data[[catagories_column]]==column_names[i],]$Proportion
        }
         
      
      }
    }
  
    return(result)
  }
  create_plotly_graph<-function(data,x,y,graph_type,graph_title,chart_style="Count"){
   
    #assume everything is already adjusted with proportions included
table<<-data
     colors <- c('rgba(255, 232, 0,1)','rgb(0, 150, 136)','rgb(63, 81, 181)',
                'rgba(212, 255, 44,1)',
                'rgba(82, 214, 136,1)', 'rgb(90, 221, 147,1)',
                'rgba(89, 199, 182,1)', 'rgb(94, 156, 147)',
                'rgb(37, 94, 101)','rgba(0,0,0,1)')
    data<-as.data.frame(data)
    
    color <- c(
    'rgb(255, 235, 59)',
    'rgb(0, 150, 136)',
    'rgb(63, 81, 181)',
    'rgb(233, 30, 99)',
    'rgb(212, 225, 87)',
    'rgb(55, 71, 79)'
    
    )
    
    #get the column names
    if(chart_style=="count"){
      y_label="Count: "
      percent=""
      track_label="Proportion: "
      
      y_column<-colnames(data)
      remove<-c("Academic_Year","Award_Type","Year_Semester","Time_to_Complete")
      y_column<-y_column[! y_column %in% remove]
      
      #save names with proportion
      track=y_column[grepl("Proportion",y_column)]
      
      #remove any column name with proportions
      y_column<-y_column[!grepl("Proportion",y_column)]
    }
    else{#want to show graph based on proportions
      
      #currently proportion is a string because it has % sign need to convert to integer
      percent="%"
       y_label="Proportion: "
       track_label="Count: "
      
      y_column<-colnames(data)
      remove<-c("Academic_Year","Award_Type","Year_Semester")
      y_column<-y_column[! y_column %in% remove]
      
      #save names with proportion
      track=y_column[!grepl("Proportion",y_column)]
      
      #remove any column name with proportions
      y_column<-y_column[grepl("Proportion",y_column)] 

      for(i in 1:length(y_column))
        data[[y_column[i]]]=as.integer(str_replace_all( data[[y_column[i]]],"%",""))
    }
    
   
    
    if(graph_type=="bar"){
       p<-data%>%plot_ly(x=data[[x]],y=data[[y_column]], type="bar",
                      marker = list(color = c('rgba(255, 232, 0,1)')),
                      hoverinfo='text',
                      text=paste(y_label,data[[y_column]],percent,"\n",track_label,data[[track]],"\nAcademic Year: ",data[[x]]),
                      insidetextfont = list(color = '#000000')
                      #The 'pull' attribute can also be used to create space between the sectors
    ) %>%
      layout(title =graph_title, margin = list(b=100,r=50,t=80),
             xaxis=list(title="" ,categoryorder ="xform"),yaxis=list(title="")
             )%>% config(displayModeBar = F)
    
    }
    else if(graph_type=="stacked"){
      if(colnames(data)[3]  %in% ethnicity_gender_levels){
        label="Ethnicity/Gender : "
      }else if(colnames(data)[3]  %in% c("Male", "Female")){
        label="Gender: "
      }else{
        label="Ethnicity: "
      }
      
      #set the layout
      p<-plot_ly(data, type='bar',x=data[[x]],y=data[[y_column[1]]],
                 hoverinfo = 'text',name=y_column[1], 
                 text=paste(y_label,data[[y_column[1]]],percent,"\n",
                            label,str_replace_all(y_column[1],"Proportion",""),"\n"
                            ,track_label,data[[track[1]]],
                            "\n Academic Year: ",data[[x]]),
                 marker = list(color = color[1]),
                insidetextfont = list(color = '#000000')
                #The 'pull' attribute can also be used to create space between the sectors
      ) %>%
        layout(title =graph_title,showlegend=T, margin = list(b=180,r=-2,t=80),
               xaxis=list(title="", categoryorder ="array",categoryarray =unique(data[[x]])),yaxis=list(title=y), barmode = 'stack'
        )%>% config(displayModeBar = F)
    
    
      if(length(y_column)>1){
        for(i in 2:length(y_column)){
          p<-add_trace(p,
                       y=data[[y_column[i]]], 
                       name=y_column[i],
                       marker = list(color = color[i]),
                       text=paste0(y_label,data[[y_column[i]]],percent,"\n",
                                   label,str_replace_all(y_column[i],"Proportion",""), "\n",
                                   track_label,data[[track[i]]],
                                   "\n Academic Year: ",data[[x]])
                      )
        }  
      }
        
    }
   
   return(p)
    
  }
  
  #get proportions for Time to Complete Award Tab
  time_to_comple_award_proportion<-function(data,cohort_data,completer_groups,gender_choice,ethnicity_choice){
    awards_time_table = NULL

    if("all_completers" %in% completer_groups)
    {
      awards_table_temp = data
      awards_table_temp$Group = rep("All completers",nrow(awards_table_temp))
      
      awards_time_table = rbind(awards_time_table,awards_table_temp)
     
    }
    if("cohort" %in% completer_groups)
    {
      awards_table_temp =cohort_data

      awards_table_temp$Group = rep("Six-year cohort",nrow(awards_table_temp))
     
      awards_time_table = rbind(awards_time_table,awards_table_temp)
    }
    
    awards_time_table$Time_to_Complete = awards_time_table$Terms_to_Complete/4
    
    if(length(gender_choice) == 0 & length(ethnicity_choice) == 0)
    {
      awards_time_table = awards_time_table %>% 
        group_by(Award_Type, Time_to_Complete, Group) %>% summarize(Count = n()) %>%
        group_by(Award_Type, Group) %>% mutate(Proportion = as.numeric(Count)/sum(as.numeric(Count))) 
    }
    else if(length(gender_choice) > 0 & length(ethnicity_choice) == 0)
    {
      awards_time_table = awards_time_table %>% filter(Gender %in% gender_choice) %>%
        group_by(Award_Type, Gender, Time_to_Complete, Group) %>% summarize(Count = n()) %>%
        group_by(Award_Type, Gender, Group) %>% mutate(Proportion = as.numeric(Count)/sum(as.numeric(Count))) 
    }
    else if(length(ethnicity_choice) > 0 & length(gender_choice) == 0)
    {
      awards_time_table = awards_time_table %>% filter(Ethnicity %in% ethnicity_choice) %>%
        group_by(Award_Type, Ethnicity, Time_to_Complete,Group) %>% summarize(Count = n()) %>%
        group_by(Award_Type, Ethnicity, Group) %>% mutate(Proportion = as.numeric(Count)/sum(as.numeric(Count))) 
    }
    else if(length(ethnicity_choice) > 0 & length(gender_choice) > 0)
    {
      awards_time_table = awards_time_table %>% filter(Ethnicity_Gender %in% levels(Ethnicity_Gender)) %>%
        group_by(Award_Type, Ethnicity_Gender, Time_to_Complete,Group) %>% summarize(Count = n()) %>%
        group_by(Award_Type, Ethnicity_Gender, Group) %>% mutate(Proportion = as.numeric(Count)/sum(as.numeric(Count))) 
    }
    
    if(length(completer_groups) <= 1)
    {
      awards_time_table$Group = NULL
    }
   
    if(nrow(awards_time_table)==0){
      return(NULL)
    }
    awards_time_table$Proportion=paste(sprintf("%.1f", round(as.numeric(awards_time_table$Proportion),3)*100),"%")
   
    
   return(awards_time_table)
  }
  
  #gets the proportions for Completers, Completers (SGEC Students), Completers (Online Students) 
  #completers_groups options:  "all_completers" or "cohort" 
  get_proportions<-function(data,cohort_data,tabCompleters,completer_groups,gender_choice,ethnicity_choice,timescale,text=NULL){
    awards_table = NULL
   
    if("all_completers" %in% completer_groups)
    {
      awards_table_temp = data
      if(length(class(data))==2 && class(data)==c("grob","gDesc"))
        awards_table_temp=NULL
      
      
      if(!is.null(awards_table_temp))
      {
        awards_table_temp$Group = rep("All completers",nrow(awards_table_temp))
        awards_table = rbind(awards_table,awards_table_temp)
      }
    }
    
    if("cohort" %in% completer_groups)
    {
      awards_table_temp = cohort_data
      if(!is.null(awards_table_temp))
      {
        awards_table_temp$Group = rep("Six-year cohort",nrow(awards_table_temp))
        awards_table = rbind(awards_table,awards_table_temp)
      }
    }
    
   
    if(length(isolate(completer_groups)) <= 1)
    {
      awards_table$Group = NULL
    }
    else
    {
      awards_table$Group = factor(awards_table$Group)
    }
    
    
    if(tabCompleters %in% c("Completers", "Time to Complete Award"))
    {
      if(length(gender_choice) > 0 & length(ethnicity_choice) == 0)#gender no ethnicity
      {
        awards_table = awards_table %>% filter(Gender %in% gender_choice) 
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) == 0) #ethnicity no gender
      {
        awards_table = awards_table %>% filter(Ethnicity %in% ethnicity_choice) 
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) > 0) #gender and ethnicity
      {
        
        awards_table = awards_table %>% filter(Ethnicity_Gender %in% levels(Ethnicity_Gender)) 
      }
    }

    if(timescale=="academic_year")
      awards_table = awards_table %>%group_by(Award_Type, Academic_Year) %>% mutate(Proportion = Count/sum(Count))
    else
      awards_table = awards_table %>%group_by(Award_Type, Year_Semester) %>% mutate(Proportion = Count/sum(Count))
    
  #  awards_table=as.data.frame(awards_table)
   
    if(nrow(awards_table)==0){
      return(NULL)
    }
    awards_table$Proportion=paste(sprintf("%.1f", round(awards_table$Proportion,3)*100),"%")
    
    
    return(awards_table)
    
    
  }
  
  change_in_completers_propotions<-function(data,tabCompleters,gender_choice,ethnicity_choice){
    change_table = data
 
    if(tabCompleters %in% c("Change in Completers over Time"))
    {
      if(length(gender_choice) > 0 & length(ethnicity_choice) == 0)
      {
        change_table = change_table %>% filter(Gender %in% gender_choice) %>%
          group_by(Award_Type, Completion_Cohort) %>% mutate(Proportion = Count/sum(Count))
        if(nrow(change_table)==0)
          return(NULL)
        change_table$Proportion=paste(sprintf("%.1f", round(change_table$Proportion,3)*100),"%")
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) == 0)
      {
        change_table = change_table %>% filter(Ethnicity %in% ethnicity_choice) %>%
          group_by(Award_Type, Completion_Cohort) %>% mutate(Proportion = Count/sum(Count))
        if(nrow(change_table)==0)
          return(NULL)
        change_table$Proportion=paste(sprintf("%.1f", round(change_table$Proportion,3)*100),"%")
        
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) > 0)
      {
        change_table = change_table %>% filter(Ethnicity_Gender %in% levels(Ethnicity_Gender)) %>%
          group_by(Award_Type, Completion_Cohort) %>% mutate(Proportion = Count/sum(Count))
        if(nrow(change_table)==0)
          return(NULL)
        change_table$Proportion=paste(sprintf("%.1f", round(change_table$Proportion,3)*100),"%")
        
      }
    }
    change_table[[".group"]]=NULL

    return(change_table)
  }
  
  change_in_efficiency_proportion<-function(data){
    efficiency_table = data
    
    efficiency_table$College = NULL
    efficiency_table$Major_Code = NULL
    efficiency_table$Term = NULL
    
    efficiency_table$FTES = round( efficiency_table$FTES, digits = 2)
    efficiency_table$FTEF = round(efficiency_table$FTEF, digits = 2) 
    efficiency_table$Efficiency = round(efficiency_table$Efficiency, digits = 2)
    
    return(efficiency_table)
  }
  
  change_time_to_complete_award_proportions<-function(data,tabCompleters,gender_choice,ethnicity_choice){
    change_time_table = data
   
    
    if(tabCompleters %in% c("Change in Time to Complete Award"))
    {
      if(length(gender_choice) == 0 & length(ethnicity_choice) == 0)
      {
        change_time_table = change_time_table %>% group_by(Award_Type, Completion_Cohort) %>%
          summarize(Group_Size = n(), Median_Years = median(change_time_table$Terms_to_Complete/4))
      }
      else if(length(gender_choice) > 0 & length(ethnicity_choice) == 0)
      {
        change_time_table = change_time_table %>% filter(Gender %in% gender_choice) %>%
          group_by(Award_Type, Completion_Cohort, Gender) %>% 
          summarize(Group_Size = n(),Median_Years = median(Terms_to_Complete/4))
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) == 0)
      {
        change_time_table = change_time_table %>% filter(Ethnicity %in% ethnicity_choice) %>%
          group_by(Award_Type, Completion_Cohort, Ethnicity) %>% 
          summarize(Group_Size = n(),Median_Years = median(Terms_to_Complete/4))
        
      }
      else if(length(ethnicity_choice) > 0 & length(gender_choice) > 0)
      {
        ethnicity_gender=NULL
        
        for(i in 1:length(ethnicity_choice)){
          ethnicity_gender=c(ethnicity_gender,paste(ethnicity_choice[i],gender_choice))
        }
        
        change_time_table = change_time_table %>% filter(Ethnicity_Gender %in% ethnicity_gender) %>%
          group_by(Award_Type, Completion_Cohort, Ethnicity_Gender) %>% 
          summarize(Group_Size = n(),Median_Years = median(Terms_to_Complete/4))
      }
    }
    
    
   return(change_time_table)
  }
  
  
  
  
  
  
  