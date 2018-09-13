library(plyr)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(RODBC)
library(gdata)
library(scales)
library(data.table)
testing<-NULL

create_graph<-function(data,tab,x,y,chart_style="Count", main_title="All Awards",mode=NULL,
                       genderChoice=NULL,ethnicityChoice=NULL){#x and y are names not vectors

  
   #assume out of order
  colors <- c('#5E879C',
              
             '#FFED85',
             
              '#5DED85',#
             
              '#5E9C93',#
              
              '#8AE60C',#
              
              '#3FCFCF',#
              
              '#D4FF2C',#
              
              '#59C7B6',
              
              '#52D688',#
              
              '#FFC300',
              
              '#E5B71B',
              
              '#9E8940',
              
              '#6B5A56',#
             
              '#000000')
  
  if(is.null(data)){
    return(data)
  }
  if(chart_style=="count")
    chart_style="Count"
  else if (chart_style=="proportion"){
    chart_style="Proportion"
    
    #get rid of percents in Proportion column
    data[[chart_style]]<-as.numeric(gsub(patter=" %",replacement = "",x=data[[chart_style]]))
  }
  
  
  
  #Every award will be a subplot
  award_type=unique(data$Award_Type)
  
  #in order for plotly to be unified with legends when plotling multiple plots in subplot must only show 1 legend
  legend=NULL
  legend[1]=T
  if(length(award_type)>1)
    for(i in 2:length(award_type))
      legend[i]=F
  
  p<-vector(length=length(award_type))
  filtered<-intersect(colnames(data),c("Gender","Ethnicity","Ethnicity_Gender"))
  
  if(tab=="Completers"){
    #could have Gender,Ethnicity, Ethnicity_Gender
    #x:Time_scale:Acadermic_Year,Year_Semester
    #y:Count or Porportion

    
    if(length(filtered)==0){#no filters were applied Ex) Gender, Ethnicity, Gender+Ethnicity
      
      data<-add_zeros_unfiltered(data,x)
      for(i in 1:length(award_type)){
        #need to remove the percent form proportion will not do anything if count
        y_data=data[data$Award_Type==award_type[i],][[y]]
        y_data=as.numeric(gsub(patter=" %",replacement = "",x=y_data))
        p[i]=create_bar_graph(x=data[data$Award_Type==award_type[i],][[x]],
                             y=y_data,
                             hover_text=paste0(y,": ",y_data,
                                               "\n",str_replace(x,"_"," "),": ",data[data$Award_Type==award_type[i],][[x]]),
                             title=award_type[i],
                             color=colors[i])
      }
      
    }
    else{#filter was applied Ex) Gender, Ethnicity, Gender+Ethnicity
      #determin the number of traces per column
      #note:not every gender or race or both is present in table when its 0 will add that for ploting purposes
      data<-complete_data_table(data,filtered,x)
     
      for(i in 1:length(award_type)){
        p[i]=create_stacked_bar_graph(data,
                                      x,
                                      y,
                                      title=award_type[i],
                                      legend=legend[i],
                                      chart_style = chart_style)
      }
      
      #create a graph by award type
      
      
    }

  }
  
  else if(tab=="Time to Complete Award"){
      #need to remove the percent form proportion will not do anything if density
    stacked=T
    legend=rep("none",length(award_type))
    legend[1]="left"
    
    if(is.null(mode)){#no filters were applied Ex) Gender, Ethnicity, Gender+Ethnicity
        if(chart_style=="histogram"){
           x="Terms_to_Complete"
           stacked=F
        }
         else
           x="`Award Type`"
      }
    else if(mode=="Gender"){
        genderChoice= genderChoice[genderChoice %in% unique(data$Gender)]
        data = data %>% filter(Gender %in% genderChoice)
        x="Gender"
        color=c(colors[1],colors[2])
      }
    else if(mode=="Ethnicity"){
        ethnicityChoice= ethnicityChoice[ethnicityChoice %in% unique(data$Ethnicity)]
        data = data %>% filter(Ethnicity %in% ethnicityChoice)
        x="Ethnicity"
      
      }
    else{
          x="`Ethnicity Gender`"
        #create a concatenated ethnicty/gender variable 
        data = data %>% filter(Gender %in% genderChoice, Ethnicity %in% ethnicityChoice) %>%
          mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender))
        
        data$Ethnicity_Gender = factor(data$Ethnicity_Gender, levels = ethnicity_gender_levels)
      }
  
      
      for(i in 1:length(award_type)){
        if(chart_style=="histogram"){
          p[i]=create_histogram(data=data[data$Award_Type==award_type[i],],
                                x=x,
                                title=award_type[i],
                                color=colors[i],
                                stacked= stacked,
                                legend=legend
                                )

        }else{
          p[i]=create_desity_graph(data=data[data$Award_Type==award_type[i],],
                                   x=x,
                                   y=y,
                                   title=award_type[i],
                                   color=colors[i])
        }
        
      } 
    
     
    
    
  }
  
  else if(tab == "Completers (SGEC Students)"){
  
    #Plotly needs zeros in data frame need to manually add them if necessary
    data = add_zeros_unfiltered2(data,x=x,"SGEC_Status")
    
    for(i in 1:length(award_type)){
      p[i]<-create_stacked_bar_graph2(data, x,y, filter_column = "SGEC_Status",title=award_type[i],legend=legend[i])
      
    }
    
  }
  
  else if(tab=="Completers (Online Students)"){
    #Plotly needs zeros in data frame need to manually add them if necessary
    data = add_zeros_unfiltered2(data,x=x,"Online_Status")
    
    for(i in 1:length(award_type)){
      p[i]<-create_stacked_bar_graph2(data, x,y, filter_column = "Online_Status",title=award_type[i],legend=legend[i])
      
    }
  }
  
  else if(tab=="Change in Completers over Time"){
    main_title=""
    if(length(filtered)==0){#no filters were applied Ex) Gender, Ethnicity, Gender+Ethnicity
    
      for(i in 1:length(award_type)){
        #need to remove the percent form proportion will not do anything if count
        y_data=data[data$Award_Type==award_type[i],][[y]]
        y_data=as.numeric(gsub(pattern =" %",replacement = "",x=y_data))
       
        p[i]=create_bar_graph(x=data[data$Award_Type==award_type[i],][[x]],
                              y=y_data,
                              hover_text=paste0(y,": ",y_data,
                                                "\n",str_replace(x,"_"," "),": ",data[data$Award_Type==award_type[i],][[x]]),
                              title=award_type[i],
                              color=colors[i])
      }
      
    }
    else{#filter was applied Ex) Gender, Ethnicity, Gender+Ethnicity
      #determin the number of traces per column
      #note:not every gender or race or both is present in table when its 0 will add that for ploting purposes
      data=add_zeros_unfiltered2(data,"Completion_Cohort",y)
      
      for(i in 1:length(award_type)){
        p[i]=create_stacked_bar_graph2(data, 
                                       x=x,
                                       y=chart_style,
                                       filter_column = filtered,
                                       title=award_type[i],
                                       legend=legend[i])
      }
      
      #create a graph by award type
      
      
    }
    
    
    
  }
  
  else if(tab=="Change in Efficiency over Time"){
    main_title=""
    #not all award types will be available need to fill in missing stuff
    completion_cohort=unique(data$Completion_Cohort)
    copy_data=as.data.frame(data)

    for(i in 1:length(award_type)){
      data_award=data[data$Award_Type==award_type[i],]
      #get all possible filters Ex)Gender,Ethnicity
      #check if all relevant data is present if not add it
      current = unique(data_award$Completion_Cohort)
      missing = completion_cohort[ !completion_cohort %in% current]

      #create empty data frame and bind to current data
      if(length(missing)>0){#iterate and append
        temp=data.frame(matrix(ncol = length(colnames(data)), nrow = length(missing)))
        colnames(temp)<-colnames(data)
        temp[is.na(temp)]<-0
        temp$Completion_Cohort<-missing
        temp$Award_Type=award_type[i]

        copy_data<-rbind(copy_data,temp)

      }
    }

    data<-copy_data[order(copy_data$Award_Type,copy_data$Completion_Cohort),]
    
    p<-create_line_graph(data, x,y, award_type=award_type)
    return(p)
  }
  
  else if(tab=="Change in Time to Complete Award"){
    main_title=""
    for(i in 1:length(award_type)){
      p[i]<-create_box_plot(data,
                            x=x,
                            y=NULL,
                            filter_column=y,
                            title=award_type[i],
                            genderChoice=genderChoice,
                            ethnicityChoice=ethnicityChoice)
    }
  }
  
  else if(tab=="Course-taking Patters"){
    main_title=""
  } 
  
  return(create_sublot(p,length(award_type),main_title))
}

title_annotation<-function(title){
  f <- list(
    family = "open sans",
    size = 14,
    color = "black")
  
  annotation <- list(
    text = title,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
}

create_sublot<-function(plots,length,title){
  rows=ceiling(length/2)
    heights=1
   if (rows==2){
    heights=c(0.6,0.4)
    }else if(rows==3)
      heights=c(0.36,0.36,0.28)
  
  p<-subplot(as.list(plots),nrows=rows,shareX=F,shareY=T,titleX=T,titleY=T,margin=c(0.02,0.02,0.02,0.25), heights=heights) %>%
    layout(title=title, margin = list( b = 100, t = 100))
  

  return(p)
}

#x and y are vectors of the same size
create_bar_graph<-function(x,y,hover_text,title, color='rgb(138, 230, 12)'){
  a<-title_annotation(title) #makes the title

  p<-plot_ly(x = x, y =y,
             type = 'bar',
             hoverinfo='text',
             text=hover_text,
             name=title,
             marker=list(color = color,
                         line = list( color = 'rgb(0, 0, 0)', width = 0)
             )
  ) %>% layout(annotations = a)%>%
    config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
           modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                         'hoverClosestCartesian','hoverCompareCartesian'
                                          ))
  return(p)
}

#x and y are names not vectors
#assume complete dataframe with 0's included
create_stacked_bar_graph<-function(data,x,y,title,legend, chart_style="Count"){
  a<-title_annotation(title) #makes the title
  award_type=title
  
  #will be filtered so get all filters
  filters<-unique(data[[y]])
  colors <- c('rgb(94, 135, 156)',
              
              'rgb(82, 214, 136,1)',#
              
              'rgba(93, 237, 133,1)',#
              
              'rgb(138, 230, 12)',#
              
              'rgb(94, 156, 147)',#
              
              'rgb(63, 207, 207)',#
              
              'rgba(89, 199, 182,1)',#
              
              'rgba(212, 255, 44,1)',
              
              'rgba(255, 232, 0,1)',
              
              'rgb(255, 195, 0)',
              
              'rgb(229, 183, 27)',
              
              'rgb(158, 137, 64)',
              
              'rgb(107, 90, 86)',#
              'rgb(0, 0, 0)')
  
  
  p<-plot_ly(x=unique(data[data[[y]]==filters[1] & data$Award_Type==award_type,][[x]]),
             y=data[data[[y]]==filters[1] & data$Award_Type==award_type,][[chart_style]],
             type="bar",
             showlegend=legend,
             legendgroup = 'group1',
             name=filters[1],
             hoverinfo='text',
            text=paste0(y,": ",filters[1],
                          "\n",chart_style,": ",data[data[[y]]==filters[1] & data$Award_Type==award_type,][[chart_style]]),
            marker = list(color = colors[1]))%>%
    layout( barmode = 'stack',annotations=a)%>%
    config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
           modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                         'hoverClosestCartesian','hoverCompareCartesian'
           ))
  
  if(length(filters)>1){#filter 1 applied above remaining filter(if any) done here
      for(i in 2:length(filters)){
       if(nrow(data[data[[y]]==filters[i] & data$Award_Type==award_type,])!=0){
           p<-p%>%add_trace(y=data[data[[y]]==filters[i] & data$Award_Type==award_type,][[chart_style]],
                           name=filters[i],
                           legendgroup = 'group1',
                           showlegend=legend,
                             hoverinfo='text',
                             text=paste0(y,": ",filters[i],
                                        "\n",chart_style,": ",data[data[[y]]==filters[i] & data$Award_Type==award_type,][[chart_style]]),
                           marker = list(color = colors[i]))
       }
      }
    }
  
  return(p)
}

#x will be Academic_Year/Year_Semester
#Y will usually be Count/Proportion
#filer_column is used to determine what stack are you adding
#assume zeros have been added Note: Plotly needs the zeros or it crashes
create_stacked_bar_graph2<-function(data,x,y,filter_column,title, legend){
  a<-title_annotation(title) #makes the title
  award_type=title
  filters=unique(data[[filter_column]])
  data[[y]]<-as.numeric(gsub(patter=" %",replacement = "",x=data[[y]]))#get rid off percents if proportion
  
  colors <- c('rgb(94, 135, 156)','rgb(82, 214, 136,1)','rgba(93, 237, 133,1)',
              'rgb(138, 230, 12)', 'rgb(94, 156, 147)','rgb(63, 207, 207)',
              'rgba(89, 199, 182,1)','rgba(212, 255, 44,1)','rgba(255, 232, 0,1)',
              'rgb(255, 195, 0)','rgb(229, 183, 27)','rgb(158, 137, 64)',
              'rgb(107, 90, 86)','rgb(0, 0, 0)')
  
  
  #Will need to get x and y based on 1) filters 2)Award_Type
  
  p<-plot_ly(x=unique(data[data[[filter_column]]==filters[1] & data$Award_Type==award_type,][[x]]),
             y=data[data[[filter_column]]==filters[1] & data$Award_Type==award_type,][[y]],
             type="bar",
             showlegend=legend,
             legendgroup = 'group1',
             name=filters[1],
             hoverinfo='text',
             text=paste0(filter_column,": ",filters[1],
                         "\n", y ,": ",data[data[[filter_column]]==filters[1] & data$Award_Type==award_type,][[y]]),
             marker = list(color = colors[1]))%>%
    layout( barmode = 'stack',annotations=a)%>%
    config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
           modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                         'hoverClosestCartesian','hoverCompareCartesian'
           ))

  if(length(filters)>1){
    for(i in 2:length(filters)){
        p<- p %>% add_trace(y = data[data[[filter_column]]==filters[i] & data$Award_Type==award_type,][[y]],
                            name = filters[i],
                            legendgroup = 'group1',
                            showlegend=legend,
                            hoverinfo='text',
                            text=paste0(filter_column,": ",filters[i],
                                       "\n", y ,": ",data[data[[filter_column]]==filters[i] & data$Award_Type==award_type,][[y]]),
                           marker = list(color = colors[i]))
      
    }
  }
 
  return(p)
}

create_box_plot<-function(data, x, y, filter_column, title, genderChoice=NULL,ethnicityChoice=NULL){
  
  colors <- c('#5E879C','#FFED85','#5DED85',
              '#5E9C93','#8AE60C','#3FCFCF',
              '#D4FF2C','#59C7B6','#52D688',
              '#FFC300','#E5B71B','#9E8940',
              '#6B5A56','#000000')
  Terms_to_Complete=data$Terms_to_Complete
  
   if(is.null(filter_column)){
      change_plot = ggplot(data, aes(x=Completion_Cohort, y = Terms_to_Complete/4)) + 
     labs(y="Terms to Complete \n", x = "\n") +
     ggtitle(as.character(title))+
     geom_boxplot(fill = "goldenrod")+
    theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_text(margin = margin(t = 20)))
   } 
   else {
    if(filter_column=="Gender"){
      data = data %>% filter(Gender %in% genderChoice)
      change_plot = ggplot(data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Gender)) + 
        labs(y="Terms to Complete \n", x = "\n") +
        ggtitle(as.character(title))+
        geom_boxplot( position=position_dodge(1)) +
        theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_text(margin = margin(t = 20)))
      
     }
    else if(filter_column=="Ethnicity"){
      data = data %>% filter(Ethnicity %in% ethnicityChoice)
      change_plot = ggplot(data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Ethnicity)) + 
        labs(y="Terms to Complete \n", x = "\n") +
        ggtitle(as.character(title))+
        geom_boxplot() +
        scale_fill_manual(values = colors,drop=FALSE)+
        theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_text(margin = margin(t = 20)))
    }
    else{
      data = data %>% filter(Ethnicity %in% ethnicityChoice, Gender %in% genderChoice) %>% 
      mutate(Ethnicity_Gender = paste0(Ethnicity," ",Gender)) 
      
      change_plot = ggplot(data, aes(x=Completion_Cohort, y = Terms_to_Complete/4, fill = Ethnicity_Gender)) + 
        labs(y="Terms to Complete \n", x = "\n") +
        ggtitle(as.character(title))+
        geom_boxplot() +
        scale_fill_manual(values = colors,drop=FALSE)+
        theme(panel.background = element_blank(), panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_text(margin = margin(t = 20)))}
   

  
     
   }
     
  

  return(ggplotly(change_plot)%>%  layout(boxmode = "group")%>%
           config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                  modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                                'hoverClosestCartesian','hoverCompareCartesian')))
}

create_histogram<-function(data,x,title,color='rgb(138, 230, 12)', stacked=F, legend="left"){
   a<-title_annotation(title) #makes the title
   
   if(stacked){#stacked when have chosen gender, ethnicity, or gender_ethnicity
     colors <- c('#5E879C','#FFED85','#5DED85',
                 '#5E9C93','#8AE60C','#3FCFCF',
                 '#D4FF2C','#59C7B6','#52D688',
                 '#FFC300','#E5B71B','#9E8940',
                 '#6B5A56','#000000')
  
     colnames(data)=gsub("_"," ",colnames(data))
     colnames(data)[11]<-"Time to Complete"
     
    print(paste("Min:",min(data[["Time to Complete"]])))
    p<- ggplot(data, aes_string("`Time to Complete`", fill = x)) + 
              labs(y="Count \n\n", x = "Time to Complete") +
               scale_x_continuous(breaks = seq(floor(min(data[["Time to Complete"]])),ceiling(max(data[["Time to Complete"]])), by = 1)) +
               geom_histogram( binwidth=ifelse(range(data[["Time to Complete"]])[2] >10 , 1 ,0.5),color="white") + 
               scale_fill_manual(values = colors )+ 
               theme(panel.background = element_blank(), panel.grid.major.x = element_blank())
     
    testing<<-p
     # p <- ggplotly(p) %>% layout(annotations = a) %>%
     #   config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
     #          modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
     #                                        'hoverClosestCartesian','hoverCompareCartesian'))
     testing<<-p
   }else{
      max_x=max(data$Time_to_Complete)+1
 
      colnames(data)[10]<-"Time_to_Complete"
      p<-plot_ly(x = data$Time_to_Complete,
                   type ="histogram",
                   name=title,
                   autobinx=F,
                   xbins=list(start=0.5, end=max_x,size=1),
                  marker=list(color = color,
                              line = list( color = 'rgb(0, 0, 0)', width = 0)
                              )
                ) %>% layout(annotations = a, bargap=0.2, xaxis=list(tickmode='linear',tick0=0, dtick=1, range=c(0,max_x))
                         )%>%config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                              modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian'))
        
  }
  

   return(p)
}


create_desity_graph<-function(data, x, y, title, color){
  colors <- c('#5E879C',
               '#FFED85',
               '#5DED85',
               '#5E9C93',
               '#8AE60C',
               '#3FCFCF',
               '#D4FF2C',
               '#59C7B6',
               '#52D688',
               '#FFC300',
               '#E5B71B',
               '#9E8940',
               '#6B5A56',
               '#000000')
  
   a<-title_annotation(title) #makes the title
  
   colnames(data)=gsub("_"," ",colnames(data))
 
  p <- ggplot(data, aes_string("`Terms to Complete`", fill = x)) +
    labs(y="Density", x = "Terms to Complete") +  scale_fill_manual(values = colors )+ 
    geom_density(alpha = 0.2) + theme(panel.background = element_blank())
  

  p <- ggplotly(p, dynamicTicks = T) %>% layout(annotations = a) %>%
    config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
           modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                         'hoverClosestCartesian','hoverCompareCartesian'))
  
  return(p)
}

#assume the percent column has been fixed
#award_type is a vector
create_line_graph<-function(data, x, y, award_type){
  colors <- c('#5E879C',
              '#FFED85',
              '#5DED85',
              '#5E9C93',
              '#8AE60C',
              '#3FCFCF',
              '#D4FF2C',
              '#59C7B6',
              '#52D688',
              '#FFC300',
              '#E5B71B',
              '#9E8940',
              '#6B5A56',
              '#000000')
  
  p<-plot_ly(data[data$Award_Type==award_type[1],], 
             x=data[data$Award_Type==award_type[1],][[x]],
             y=data[data$Award_Type==award_type[1],][[y]],
             name= award_type[1],
             type='scatter',
             mode='lines+markers',
             marker=list(color = colors[1]),
             hoverinfo='text',
             text=paste0("Completion Cohort: ", data[data$Award_Type==award_type[1],]$Completion_Cohort,
                         "\n Efficiency: ", data[data$Award_Type==award_type[1],]$Efficiency)
             )%>% layout(margin = list( b = 100, t = 50, pad = 4))%>%
                  config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
                  modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                                'hoverClosestCartesian','hoverCompareCartesian'))

  
  if(length(award_type)>1){
    for(i in 2:length(award_type)){
      p<- p %>% add_trace(y=data[data$Award_Type==award_type[i],][[y]],
                          name=award_type[i],
                          mode= 'lines+markers',
                          marker=list(color = colors[i]),
                          hoverinfo='text',
                          text=paste0("Completion Cohort: ", data[data$Award_Type==award_type[i],]$Completion_Cohort,
                                      "\n Efficiency: ", data[data$Award_Type==award_type[i],]$Efficiency))
    }
  }
  return(p)
}


#used to make table bigger to accomodate for 0s in data that are not currently present
complete_data_table<-function(data, filter_type,x_axis){


  #need to account for diffrent award_types
  award_types=unique(data$Award_Type) 
  years=unique(data[[x_axis]])
  filters=unique(data[[filter_type]])
  for(i in 1:length(award_types)){
    data_award=data[data$Award_Type==award_types[i],]
    #get all possible filters Ex)Gender,Ethnicity
    
    for(j in 1:length(filters)){
      #check if all relevant data is present if not add it
      current = unique(data_award[data_award[[filter_type]]==filters[j],][[x_axis]])
      missing = years[ !years %in% current]
      
      #create empty data frame and bind to current data
      if(length(missing)>0){#iterate and append
        temp=data.frame(matrix(ncol = length(colnames(data)), nrow = length(missing)))
        colnames(temp)<-colnames(data) #copy column names of original dataframe to new one
        temp[is.na(temp)]<-0 #make everything equal to zero
        
        temp[[x_axis]]<-missing #add missing years or semesters
        
        temp$Award_Type=award_types[i] #add award type
      
        temp[[filter_type]]=filters[j] #add filter
      
        data<-rbind(as.data.frame(data),temp) #combine to original dataframe
      }
      
    }
  }
  

  data<-data[order(data[["Award_Type"]],data[[filter_type]],data[[x_axis]]),]

  return(data)
}

#this function is used assuming the filters are on the column names
#Ex) Academic_Year, Award_Type, Female , Male, Female_Proportion, Male_Proportion
add_zeros_unfiltered<-function(data,x){
  #add leftover year 
  data[[".group"]]<-NULL
  copy_data=as.data.frame(data)
  award_types=unique(data$Award_Type) 
  years=unique(data[[x]])
  
  for(i in 1:length(award_types)){
    data_award=data[data$Award_Type==award_types[i],]
    #get all possible filters Ex)Gender,Ethnicity
      #check if all relevant data is present if not add it
      current = unique(data_award[[x]])
      missing = years[ !years %in% current]
      
      #create empty data frame and bind to current data
      if(length(missing)>0){#iterate and append
        temp=data.frame(matrix(ncol = length(colnames(data)), nrow = length(missing)))
        colnames(temp)<-colnames(data)
        temp[is.na(temp)]<-0
        temp[[x]]<-missing
        temp$Award_Type=award_types[i]
        
       
        copy_data<-rbind(copy_data,temp)
        
      
    }
  }

  data<-copy_data[order(copy_data[["Award_Type"]],copy_data[[x]]),]
  
  return(data)
}

#function used to fill in gaps. x is typically Academic_Year/Year_Semester and filter can be column wit the filters
# filter Ex)Ethnicity, Gender, Ethnicity_Gender, SGEC_Status, Online_Status
add_zeros_unfiltered2<-function(data,x, filter){
  #add leftover year 
  data[[".group"]]<-NULL
  copy_data=as.data.frame(data)
  award_types=unique(data$Award_Type) 
  years=unique(data[[x]])
  
  #get filters
  filters=unique(data[[filter]])
  
  for(i in 1:length(award_types)){
    for(j in 1:length(filters)){
        data_award=data[data$Award_Type==award_types[i] & data[[filter]]==filters[j],]
    
        current = unique(data_award[[x]])
        missing = years[ !years %in% current]
        
        #create empty data frame and bind to current data
        if(length(missing)>0){#iterate and append
          temp=data.frame(matrix(ncol = length(colnames(data)), nrow = length(missing)))
          colnames(temp)<-colnames(data)
          temp[is.na(temp)]<-0
          temp[[x]]<-missing
          temp$Award_Type=award_types[i]
          temp[[filter]]<-filters[j]
          copy_data<-rbind(copy_data,temp)
        }
    }
    
  }
  
  data<-copy_data[order(copy_data[["Award_Type"]],copy_data[[x]]),]
  
  return(data)
}


get_noti=function(){
  notification <- notificationItem(icon = icon("info-circle"), status = "info", paste0("Tutorial"))
  notification$children[[1]] <- a(href="#shiny-tab-main","onclick"=paste0("clickFunction('",
                                                                          paste0(substr(as.character(runif(1, 0, 1)),1,6),
                                                                                 "Tutorial"),"'); return false;"),
                                  list(notification$children[[1]]$children))
  return(notification)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}