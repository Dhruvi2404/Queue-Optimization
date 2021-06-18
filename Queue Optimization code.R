## Variable Declaration

m_tol=100

sd_tol=0.5

m_picking=40

sd_picking=4

max_cart=40

max_counters=5

store_close_time=54000 #15 hours


cust_time_m<-c(5,7,9,11,13,15,17,20,17,15,13,11,9,7,5)

cust_time_sd<-c(2,2,3,3,3,4,4,5,5,3,3,3,2,2,2,2)

names(cust_time_m)<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)

names(cust_time_sd)<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)

go_to_queue=c()

go_to_queue2=c()

cust_in=c()

cust_in2=c()

counterT = 0 # Optimization initializations packing_m = 5

packing_sd = 0.5

q_packing_rates=abs(rnorm(6,packing_m,packing_sd))

q_data = list(c(),c(),c(),c(),c(),c())

q_s = c(100,100,100,100,100,100)

q_num_ppl = c(0,0,0,0,0,0)

q_cum_carts = c(0,0,0,0,0,0)

for(Time in 1:55000)#54000seconds
{
  if(Time<=51000){
    time_counter=time_counter+1
    if (time_counter==300){
      time_counter<-0
      num_of_cust<-abs(as.integer(rnorm(1,cust_time_m[toString(Time%/%3600)],
                                        cust_time_sd[toString(Time%/%3600)])))
      cust_in=initialization(num_of_cust,m_tol=m_tol,sd_tol=sd_tol,m_picking=m_picking,
                             sd_picking=sd_picking,max_cart=max_cart,time=Time,exit_time1=-1,queue_number1=-1)
      #Vector of Customers entering the store 
      #New customers coming in every 5 mins
      cust_in2<-append(cust_in2,cust_in)
      
    }
    
  }
  remove=c()
  for (customer in 1:length(cust_in2)){
    if(length(cust_in2)>1){
      if(cust_in2[[customer]]@shopping_time>=Time){
        #shopping_time = enter_time+picking_rate*cart_size
        cust_in2[[customer]]@status=2
        cust_in2[[customer]]@tolerance=cust_in2[[customer]]@tolerance-0.0085*cust_in2[[customer]]@picking_time
        go_to_queue=append(go_to_queue,cust_in2[[customer]])#go_to_queue is for group 3
        remove=append(remove,customer)
        go_to_queue2 = append(go_to_queue2, go_to_queue)
        
        
      }
    }
    
  }
  if( length(remove)>1){
    if(length(cust_in2)>1){
      cust_in2=cust_in2[-(remove)]
    }}
  
  # This is inside the main time loop
  # Popping customers who are finished billing before adding incoming customers
  for(i in 1:6){
    if(q_num_ppl[i]==0){
      next
    } else {
      
      x = q_data[[i]][[1]]
      if(x@exit_time <= Time){
        q_data[[i]] = q_data[[i]][2:q_num_ppl[[i]]]# Popping the first customer from the queue
        #cat("person removed from queue ",x@queue_number," at time ",Time)
        #cat("\n")
        q_num_ppl[i] = q_num_ppl[i]-1
        q_cum_carts[i] = q_cum_carts[i] - x@cart_size
        if(q_num_ppl[i]<=1){
          q_s[i] = 100
        }else{
          wt = waitTime(num_queue = q_num_ppl[i],
                        cum_cart = q_cum_carts[i],
                        packing_t = q_packing_rates[i])
          q_s[i] = customerSatisfaction(wt,q_data[[i]][[q_num_ppl[i]]]@tolerance) 
        }
      } 
    }
  }
  
  
  #cat(length(go_to_queue),'  ')
  current_cust = go_to_queue
  
  
  
  # Assuming current_cust contains incoming customers at that particular time 't'
  if(class(current_cust)!="NULL"){
    for(i in 1:length(current_cust)){
      for(j in 1:6){
        if(q_s[j]>=70){
          if (class(current_cust[[i]])=='NULL'){
            next
          }
          q_data[[j]] = c(q_data[[j]],current_cust[[i]]) # Pushing a customer at the end of the queue
          #cat("person added to queue ",j," at time ",Time)
          #cat("\n")
          q_num_ppl[j] = q_num_ppl[j] + 1
          q_cum_carts[j] = q_cum_carts[j] + current_cust[[i]]@cart_size
          current_cust[[i]]@queue_number = j
          q_data[[j]][[q_num_ppl[j]]]@queue_number = j
          if(q_num_ppl[j]==1){
            q_s[j] = 100
            current_cust[[i]]@exit_time = exitTime(join_queue_time = current_cust[[i]]@shopping_time,
                                                   cartSize = current_cust[[i]]@cart_size,
                                                   packing_t = q_packing_rates[j])
            q_data[[j]][[q_num_ppl[j]]]@exit_time = exitTime(join_queue_time = current_cust[[i]]@shopping_time,
                                                             cartSize = current_cust[[i]]@cart_size,
                                                             packing_t = q_packing_rates[j])
          }else{
            wt = waitTime(num_queue = q_num_ppl[j],
                          cum_cart = q_cum_carts[j],
                          packing_t = q_packing_rates[j])
            q_s[j] = customerSatisfaction(wt,current_cust[[i]]@tolerance)
            current_cust[[i]]@exit_time =exitTime(join_queue_time = current_cust[[i]]@shopping_time,
                                                  wait_time = wt,
                                                  cartSize = current_cust[[i]]@cart_size,
                                                  packing_t = q_packing_rates[j])
            q_data[[j]][[q_num_ppl[j]]]@exit_time = exitTime(join_queue_time = current_cust[[i]]@shopping_time,
                                                             wait_time = wt,
                                                             cartSize = current_cust[[i]]@cart_size,
                                                             packing_t = q_packing_rates[j])
          }
        }
      }
    }
    
  }
  temp2=c()
  for( n in 1:6){
    for (m in 1:length(q_data[[n]])){
      if(class(q_data[[n]][[m]])=='NULL'){
        temp2=c(temp2,m)
      }
      q_data[[n]][-m]
    }}
  for (hnm in length(current_cust)){
    if( class(current_cust[[hnm]])=='NULL'){
      current_cust[-hnm]
    }
  }
  
  if(counterT<100){
    counterT = counterT+1
  } else {
    cat("Queues at time ", Time, c(q_num_ppl[1],q_num_ppl[2],q_num_ppl[3],q_num_ppl[4],q_num_ppl[5],q_num_ppl[6]))
    cat("\n")
    counterT = 0
    # show the number of people at any counter at time 't' / checking counter status
  }
  
  
  
  
  # Updations
  go_to_queue = c()
  
}