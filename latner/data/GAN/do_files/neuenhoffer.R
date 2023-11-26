library(torch)
# source("gan_functions.R")
setwd("/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/GAN/")


device <- torch_device("cpu")
Generator <- torch::nn_module(
  initialize = function(noise_dim,
                        data_dim,
                        hidden_units = list(1),
                        dropout_rate = 0,
                        sigmoid = FALSE) {
    # Initialize an empty nn_sequential module
    self$seq <- torch::nn_sequential()
    
    # For the hidden layers we need to keep track of our input and output dimensions. The first input will be our noise vector, therefore, it will be noise_dim
    dim <- noise_dim
    
    # i will be a simple counter to keep track of our network depth
    i <- 1
    
    # Now we loop over the list of hidden units and add the hidden layers to the nn_sequential module
    if (length(hidden_units) > 0) {
      for (neurons in hidden_units) {
        # First, we add a ResidualBlock of the respective size.
        self$seq$add_module(module =  torch::nn_linear(dim, neurons),
                            name = paste0("Linear_", i))
        
        # Add a leakyReLU activation
        self$seq$add_module(module = torch::nn_leaky_relu(),
                            name = paste0("Activation_", i))
        # And then a Dropout layer.
        self$seq$add_module(
          module = torch::nn_dropout(dropout_rate),
          name = paste0("Dropout_", i)
        )
        # Now we update our dim for the next hidden layer.
        # Since it will be another ResidualBlock the input dimension will be dim+neurons
        dim <- neurons
        # Update the counter
        i <- i + 1
      }
    }
    # Finally, we add the output layer. The output dimension must be the same as our data dimension (data_dim).
    self$seq$add_module(module = torch::nn_linear(dim, data_dim),
                        name = "Output")
    
    if (sigmoid) {
      self$seq$add_module(module = torch::nn_sigmoid(),
                          name = "Sigmoid_Output")
    }
    
  },
  forward = function(input) {
    input <- self$seq(input)
    input
  }
)

Residual <- torch::nn_module(
  initialize = function(i, o) {
    self$fc <- nn_linear(i, o)
    self$bn <- nn_batch_norm1d(o)
    #self$relu <- nn_leaky_relu()
    self$relu <- nn_relu()
    
  },
  forward = function(input) {
    out <- self$fc(input)
    out <- self$bn(out)
    out <- self$relu(out)
    torch_cat(list(out, input), dim = 2)
  }
)

Residual_D <- torch::nn_module(
  initialize = function(i, o) {
    self$fc <- nn_linear(i, o)
    #self$bn <- nn_batch_norm1d(o)
    self$relu <- nn_leaky_relu()
    #self$relu <- nn_relu()
    
  },
  forward = function(input) {
    out <- self$fc(input)
    #out <- self$bn(out)
    out <- self$relu(out)
    torch_cat(list(out, input), dim = 2)
  }
)



CTGAN_Generator <- torch::nn_module(
  initialize = function(noise_dim,
                        data_dim,
                        hidden_units = list(256, 256),
                        dropout_rate = 0) {
    # Initialize an empty nn_sequential module
    self$seq <- torch::nn_sequential()
    
    # For the hidden layers we need to keep track of our input and output dimensions. The first input will be our noise vector, therefore, it will be noise_dim
    dim <- noise_dim
    
    # i will be a simple counter to keep track of our network depth
    i <- 1
    
    # Now we loop over the list of hidden units and add the hidden layers to the nn_sequential module
    if (length(hidden_units) > 1) {
      for (neurons in hidden_units) {
        # First, we add a ResidualBlock of the respective size.
        self$seq$add_module(module = Residual(dim, neurons),
                            name = paste0("Residual", i))
        
        
        # Now we update our dim for the next hidden layer.
        # Since it will be another ResidualBlock the input dimension will be dim+neurons
        dim <- dim + neurons
        # Update the counter
        i <- i + 1
      }
    }
    # Finally, we add the output layer. The output dimension must be the same as our data dimension (data_dim).
    self$seq$add_module(module = torch::nn_linear(dim, data_dim),
                        name = "Output")
  },
  forward = function(input) {
    input <- self$seq(input)
    input
  }
)

Res_Discriminator <- torch::nn_module(
  initialize = function(data_dim,
                        hidden_units = list(256, 256),
                        dropout_rate = 0.5,
                        sigmoid = TRUE,
                        pac = 1) {
    # Initialize an empty nn_sequential module
    self$seq <- torch::nn_sequential()
    
    # For the hidden layers we need to keep track of our input and output dimensions. The first input will be our noise vector, therefore, it will be noise_dim
    self$pacdim <- data_dim * pac
    dim <- self$pacdim
    
    # i will be a simple counter to keep track of our network depth
    i <- 1
    
    # Now we loop over the list of hidden units and add the hidden layers to the nn_sequential module
    if (length(hidden_units) > 1) {
      for (neurons in hidden_units) {
        # First, we add a ResidualBlock of the respective size.
        self$seq$add_module(module = Residual_D(dim, neurons),
                            name = paste0("Residual", i))
        
        
        # Now we update our dim for the next hidden layer.
        # Since it will be another ResidualBlock the input dimension will be dim+neurons
        dim <- dim + neurons
        # Update the counter
        i <- i + 1
      }
    }
    # Finally, we add the output layer. The output dimension must be the same as our data dimension (data_dim).
    self$seq$add_module(module = torch::nn_linear(dim, data_dim),
                        name = "Output")
    
    if (sigmoid) {
      self$seq$add_module(module = torch::nn_sigmoid(),
                          name = "Sigmoid_Output")
    }
  },
  forward = function(input) {
    data <- self$seq(input$view(list(-1, self$pacdim)))
    data
  }
)

Discriminator <- torch::nn_module(
  initialize = function(data_dim,
                        hidden_units = list(256, 256),
                        dropout_rate = 0.5,
                        sigmoid = T,
                        pac = 1) {
    # Initialize an empty nn_sequential module
    self$seq <- torch::nn_sequential()
    
    # For the hidden layers we need to keep track of our input and output dimensions. The first input will be our noise vector, therefore, it will be noise_dim
    self$pacdim <- data_dim * pac
    dim <- self$pacdim
    # i will be a simple counter to keep track of our network depth
    i <- 1
    
    # Now we loop over the list of hidden units and add the hidden layers to the nn_sequential module
    if (length(hidden_units) > 1) {
      for (neurons in hidden_units) {
        # We start with a fully connected linear layer
        self$seq$add_module(module = torch::nn_linear(dim, neurons),
                            name = paste0("Linear_", i))
        # Add a leakyReLU activation
        self$seq$add_module(module = torch::nn_leaky_relu(),
                            name = paste0("Activation_", i))
        # And a Dropout layer
        # self$seq$add_module(module = torch::nn_dropout(dropout_rate),
        #                     name = paste0("Dropout_", i))
        # Update the input dimension to the next layer
        dim <- neurons
        # Update the counter
        i <- i + 1
      }
    }
    # Add an output layer to the net. Since it will be one score for each example we only need a dimension of 1.
    self$seq$add_module(module = torch::nn_linear(dim, 1),
                        name = "Output")
    if (sigmoid) {
      self$seq$add_module(module = torch::nn_sigmoid(),
                          name = "Sigmoid_Output")
    }
    
  },
  forward = function(input) {
    data <- self$seq(input$view(list(-1, self$pacdim)))
    data
  }
)


CTGAN_Discriminator <- torch::nn_module(
  initialize = function(data_dim,
                        hidden_units = list(256, 256),
                        dropout_rate = 0.5,
                        sigmoid = FALSE,
                        pac = 1) {
    # Initialize an empty nn_sequential module
    self$seq <- torch::nn_sequential()
    
    # For the hidden layers we need to keep track of our input and output dimensions. The first input will be our noise vector, therefore, it will be noise_dim
    self$pacdim <- data_dim * pac
    dim <- self$pacdim
    # i will be a simple counter to keep track of our network depth
    i <- 1
    
    # Now we loop over the list of hidden units and add the hidden layers to the nn_sequential module
    if (length(hidden_units) > 1) {
      for (neurons in hidden_units) {
        # We start with a fully connected linear layer
        self$seq$add_module(module = torch::nn_linear(dim, neurons),
                            name = paste0("Linear_", i))
        # Add a leakyReLU activation
        self$seq$add_module(module = torch::nn_leaky_relu(0.2),
                            name = paste0("Activation_", i))
        # And a Dropout layer
        self$seq$add_module(
          module = torch::nn_dropout(dropout_rate),
          name = paste0("Dropout_", i)
        )
        # Update the input dimension to the next layer
        dim <- neurons
        # Update the counter
        i <- i + 1
      }
    }
    # Add an output layer to the net. Since it will be one score for each example we only need a dimension of 1.
    self$seq$add_module(module = torch::nn_linear(dim, 1),
                        name = "Output")
    if (sigmoid) {
      self$seq$add_module(module = torch::nn_sigmoid(),
                          name = "Sigmoid_Output")
    }
    
  },
  
  calc_gradient_penalty = function(real_data,
                                   fake_data,
                                   device = "cpu",
                                   pac = 10,
                                   lambda = 10) {
    alpha <-
      torch::torch_rand(real_data$size(1) %/% pac, 1, 1, device = device)
    alpha <- alpha$`repeat`(c(1, pac, real_data$size(2)))
    alpha <- alpha$view(c(-1, real_data$size(2)))
    
    interpolates <- alpha * real_data + ((1 - alpha) * fake_data)
    
    disc_interpolates <- self$forward(interpolates)
    
    gradients <- torch::autograd_grad(
      outputs = disc_interpolates,
      inputs = interpolates,
      grad_outputs = torch::torch_ones_like(disc_interpolates, device = device),
      create_graph = TRUE,
      retain_graph = TRUE
    )[[1]]
    
    gradients_view <-
      gradients$view(c(-1, pac * real_data$size(2)))$norm(2, dim = 2) - 1
    gradient_penalty <- ((gradients_view) ^ 2)$mean() * lambda
    
    return(gradient_penalty)
    
  },
  
  forward = function(input) {
    data <- self$seq(input$view(list(-1, self$pacdim)))
    data
  }
)

sample_normal <-
  function(mb_size, embedding_dim = 1, device)
    torch::torch_tensor(array(rnorm(mb_size * embedding_dim), c(mb_size, embedding_dim)))$to(device = device)


sample_uniform <-
  function(mb_size, embedding_dim = 1, device)
    torch::torch_tensor(array(runif(mb_size * embedding_dim), c(mb_size, embedding_dim)))$to(device = device)


dgp <-
  function(mb_size, device)
    torch::torch_tensor(array(rnorm(mb_size, mean = 2), c(mb_size, 1)))$to(device = device)



GAN_value_fct <- function(real_scores, fake_scores) {
  d_loss <-
    torch::torch_log(real_scores) + torch::torch_log(1 - fake_scores)
  d_loss <- -d_loss$mean()
  
  
  g_loss <- torch::torch_log(1 - fake_scores)
  
  g_loss <- g_loss$mean()
  
  return(list(d_loss = d_loss,
              g_loss = g_loss))
  
}

rel_GAN_value_fct <- function(real_scores, fake_scores) {
  BCE_stable <- torch::nn_bce_with_logits_loss()
  
  y <- torch::torch_ones_like(real_scores)
  d_loss <- BCE_stable(real_scores - fake_scores, y)
  
  g_loss <- BCE_stable(fake_scores - real_scores, y)
  return(list(d_loss = d_loss,
              g_loss = g_loss))
}

rel_avg_GAN_value_fct <- function(real_scores, fake_scores) {
  BCE_stable <- torch::nn_bce_with_logits_loss()
  
  y <- torch::torch_ones_like(real_scores)
  y2 <- torch::torch_zeros_like(fake_scores)
  d_loss <-
    (
      BCE_stable(real_scores - torch::torch_mean(fake_scores), y) + BCE_stable(fake_scores - torch::torch_mean(real_scores), y2)
    ) / 2
  
  g_loss <-
    (
      BCE_stable(real_scores - torch::torch_mean(fake_scores), y2) + BCE_stable(fake_scores - torch::torch_mean(real_scores), y)
    ) / 2
  return(list(d_loss = d_loss,
              g_loss = g_loss))
}

rel_avg_LSGAN_value_fct <- function(real_scores, fake_scores) {
  y <- torch::torch_ones_like(real_scores)
  
  d_loss <-
    (torch::torch_mean((
      real_scores - torch::torch_mean(fake_scores) - y
    ) ^ 2) + torch::torch_mean((
      fake_scores - torch::torch_mean(real_scores) + y
    ) ^ 2)) / 2
  
  g_loss <-
    (torch::torch_mean((
      real_scores - torch::torch_mean(fake_scores) + y
    ) ^ 2) + torch::torch_mean((
      fake_scores - torch::torch_mean(real_scores) - y
    ) ^ 2)) / 2
  return(list(d_loss = d_loss,
              g_loss = g_loss))
}


WGAN_weight_clipper <- function(d_net, clip_values = c(-1, 1)) {
  for (parameter in names(d_net$parameters)) {
    d_net$parameters[[parameter]]$data()$clip_(clip_values[1], clip_values[2])
  }
}

WGAN_value_fct <- function(real_scores, fake_scores) {
  d_loss <-
    torch::torch_mean(real_scores) - torch::torch_mean(fake_scores)
  d_loss <- -d_loss$mean()
  
  
  g_loss <- torch::torch_mean(fake_scores)
  
  g_loss <- -g_loss$mean()
  
  return(list(d_loss = d_loss,
              g_loss = g_loss))
  
}




run_experiment <-
  function(pop_size = 1000,
           mb_size = NULL,
           n_steps = 500,
           real_data = NULL,
           data_mu = 2,
           data_sigma = 1,
           data_dim = 1,
           embedding_dim = 1,
           seed = NULL,
           pac = 1,
           d_arch = NULL,
           g_arch = NULL,
           d_hidden_units = list(256, 256),
           g_hidden_units = list(1),
           adam_betas = c(0.9, 0.999),
           g_lr = 0.01,
           d_lr = 0.01,
           d_decay = 0,
           g_decay = 0,
           d_sigmoid = TRUE,
           GAN_value_fct = GAN_value_fct,
           CTGAN = FALSE,
           sample_noise = sample_normal,
           ...) {
    fct_input <- c(as.list(environment()), list(...))
    library(torch)
    if (is.null(seed)) {
      x <- .Random.seed
      torch::torch_manual_seed(x[2])
    } else {
      .Random.seed <- seed
      x <- .Random.seed
      torch::torch_manual_seed(seed)
    }
    
    if (is.null(mb_size)) {
      mb_size <- pop_size
    }
    if (!is.null(real_data)) {
      pop_size <- nrow(real_data)
      data_dim <- ncol(real_data)
    }
    
    
    if (CTGAN) {
      generator <-
        CTGAN_Generator(noise_dim = embedding_dim,
                        data_dim = data_dim,
                        hidden_units = g_hidden_units)$to(device = device)
      
      
      discriminator <-
        CTGAN_Discriminator(
          data_dim = data_dim,
          hidden_units = d_hidden_units,
          sigmoid = d_sigmoid,
          pac = pac
        )$to(device = device)
    } else {
      if (is.null(d_arch)) {
        discriminator <-
          Discriminator(
            data_dim = data_dim,
            hidden_units = d_hidden_units,
            sigmoid = d_sigmoid,
            pac = pac
          )$to(device = device)
      } else {
        discriminator <- d_arch(
          data_dim = data_dim,
          hidden_units = d_hidden_units,
          sigmoid = d_sigmoid,
          pac = pac
        )$to(device = device)
      }
      
      if (is.null(g_arch)) {
        generator <-
          Generator(
            noise_dim = embedding_dim,
            data_dim = data_dim,
            hidden_units = g_hidden_units
          )$to(device = device)
      } else {
        generator <- g_arch(
          noise_dim = embedding_dim,
          data_dim = data_dim,
          hidden_units = g_hidden_units
        )$to(device = device)
      }
    }
    
    g_optim <-
      torch::optim_adam(
        generator$parameters,
        lr = g_lr,
        betas = adam_betas,
        weight_decay = g_decay
      )
    d_optim <-
      torch::optim_adam(
        discriminator$parameters,
        lr = d_lr,
        betas = adam_betas,
        weight_decay = d_decay
      )
    
    # g_optim <-
    #   torch::optim_sgd(generator$parameters, lr = g_lr)
    # d_optim <-
    #   torch::optim_sgd(discriminator$parameters, lr = d_lr)
    
    
    
    #res_g <- sapply(simple_gen$parameters, torch::as_array)
    g_losses <- NULL
    d_losses <- NULL
    if (is.null(real_data)) {
      real_data_pop <-
        sample_normal(
          pop_size,
          data_dim = data_dim,
          mu = data_mu,
          sigma = data_sigma,
          device
        )
    } else {
      real_data_pop <- torch::torch_tensor(real_data)
    }
    
    for (i in 1:n_steps) {
      mb_id <- sample(pop_size, mb_size, replace = FALSE)
      real_data <- real_data_pop[mb_id, ]
      
      # Get a fresh noise sample -------------------------------------------------
      z <-
        sample_noise(mb_size, embedding_dim = embedding_dim, device)
      # Produce fake data from noise ---------------------------------------------
      fake_data <- generator(input = z)
      # Compute the discriminator scores on real and fake data -------------------
      dis_real <- discriminator(real_data)
      dis_fake <- discriminator(fake_data)
      # Calculate the discriminator loss
      d_loss <- GAN_value_fct(dis_real, dis_fake)[["d_loss"]]
      # if(i %% 10 == 0){
      #   d_loss <- GAN_value_fct(dis_fake, dis_real)[["d_loss"]]
      # }
      # Clip weights according to weight_clipper ---------------------------------
      #weight_clipper(d_net)
      # What follows is one update step for the discriminator net-----------------
      # First set all previous gradients to zero
      
      d_losses <- c(d_losses, as_array(d_loss))
      
      if (CTGAN) {
        pen <-
          discriminator$calc_gradient_penalty(real_data, fake_data, device, pac)
      }
      
      d_optim$zero_grad()
      
      if (CTGAN) {
        pen$backward(retain_graph = TRUE)
      }
      
      
      # Pass the loss backward through the net
      d_loss$backward()
      
      # Take one step of the optimizer
      d_optim$step()
      
      #WGAN_weight_clipper(simple_dis)
      
      # Update the generator -----------------------------------------------------
      mb_id <- sample(pop_size, mb_size, replace = FALSE)
      real_data <- real_data_pop[mb_id, ]
      
      # Get a fresh noise sample -------------------------------------------------
      z <-
        sample_noise(mb_size, embedding_dim = embedding_dim, device)
      # Produce fake data --------------------------------------------------------
      fake_data <- generator(z)
      
      # Calculate discriminator score for fake data ------------------------------
      dis_real <- discriminator(real_data)
      dis_fake <- discriminator(fake_data)
      # Get generator loss based on scores ---------------------------------------
      g_loss <- GAN_value_fct(dis_real, dis_fake)[["g_loss"]]
      # What follows is one update step for the generator net --------------------
      # First set all previous gradients to zero
      g_losses <- c(g_losses, as_array(g_loss))
      g_optim$zero_grad()
      
      # Pass the loss backward through the net
      g_loss$backward()
      
      # Take one step of the optimizer
      g_optim$step()
      
      #print(simple_gen$parameters)
      
      # res_g <-
      #   rbind(res_g, sapply(simple_gen$parameters, torch::as_array))
    }
    
    res_list <-
      list(#generators = res_g,
        g_losses = g_losses,
        d_losses = d_losses,
        generator = generator)
    attr(res_list, "seed") <- x
    attr(res_list, "input") <- fct_input
    
    return(res_list)
    
  }


show_results <-
  function(res,
           real_data,
           sample_noise,
           device,
           trial_name = NULL,
           folder = NULL,
           save_plots = FALSE) {
    if (is.null(trial_name)) {
      trial_name <- paste0(round(as.numeric(Sys.time()), 0), "_GAN_trial")
    }
    
    if (save_plots) {
      pdf(
        file = paste0(
          folder,
          ifelse(is.null(folder), "", "/"),
          trial_name,
          "_losses.pdf"
        ),
        width = 16,
        height = 9
      )
    }
    par(mfrow = c(1, 1))
    
    plot(
      res$g_losses,
      type = "l",
      ylim = c(min(c(
        res$g_losses, res$d_losses
      )), max(c(
        res$g_losses, res$d_losses
      ))),
      col = viridis::viridis(3, 0.8)[1],
      bty = "n",
      las = 1,
      xlab = "Training Step",
      ylab = "Loss",
      main = paste0(trial_name, "\nGenerator and Discriminator Losses")
      
    )
    lines(res$d_losses, col = viridis::viridis(3, 0.8)[2])
    legend(
      "bottomright",
      legend = c("Generator", "Discriminator"),
      col = viridis::viridis(3)[1:2],
      lty = "solid",
      bty = "n"
    )
    if (save_plots) {
      dev.off()
    }
    .Random.seed <- attr(res, "seed")
    attrs <- attr(res, "input")
    # real_data <-
    #   as_array(
    #     sample_normal(
    #       attrs$pop_size,
    #       data_dim = attrs$data_dim,
    #       mu = attrs$data_mu,
    #       sigma = attrs$data_sigma,
    #       device = device
    #     )$detach()$cpu()
    #   )
    
    
    #real_data <- trans_data
    z <-
      sample_noise(nrow(real_data),
                   embedding_dim = attrs$embedding_dim,
                   device = device)
    
    synth_data <- as_array(res$generator(z)$detach()$cpu())
    
    cat("\nReal Means: ", colMeans(real_data))
    cat("\nSynthetic Means: ", colMeans(synth_data))
    cat("\nReal Correlations\n")
    print(cor(real_data))
    cat("\nSynthetic Correlations\n")
    print(cor(synth_data))
    
    if (save_plots) {
      pdf(
        file = paste0(
          folder,
          ifelse(is.null(folder), "", "/"),
          trial_name,
          "_correlations.pdf"
        ),
        width = 16,
        height = 9
      )
    }
    par(mfrow = c(1, 2), oma = c(0, 0, 5, 0))
    corrplot::corrplot(cor(real_data), tl.col = "black", method = "color",addCoef.col = "black", col = viridis::viridis(200), main = "Real Data", number.cex = 0.5, number.font = 1, cex.main = 0.8, mar = c(0,0,1,0))
    corrplot::corrplot(cor(synth_data), tl.col = "black", method = "color",addCoef.col = "black", col = viridis::viridis(200), main = "Synthetic Data", number.cex = 0.5, number.font = 1, cex.main = 0.8, mar = c(0,0,1,0))
    mtext(paste0(trial_name, "\nCorrelations"), outer = TRUE, font = 2)
    if(save_plots){
      dev.off() 
    }
    if (save_plots) {
      pdf(
        file = paste0(
          folder,
          ifelse(is.null(folder), "", "/"),
          trial_name,
          "_distributions.pdf"
        ),
        width = 16,
        height = 16
      )
    }
    
    if(ncol(real_data) < 12){
      par(mfrow = c(4, 3), oma = c(0, 0, 4, 0))
    }
    if(ncol(real_data) >= 12 & ncol(real_data) < 16){
      par(mfrow = c(4, 4))
    }
    if(ncol(real_data) >= 16 & ncol(real_data) < 20){
      par(mfrow = c(5, 4))
    }
    if(ncol(real_data) >= 20 & ncol(real_data) < 25){
      par(mfrow = c(5, 5))
    }
    
    sapply(1:ncol(real_data), function(x) {
      dens_real <- density(real_data[, x])
      dens_fake <- density(synth_data[, x])
      
      xlims <-
        c(min(c(dens_real$x, dens_fake$x)), max(c(dens_real$x, dens_fake$x)))
      ylims <-
        c(min(c(dens_real$y, dens_fake$y)), max(c(dens_real$y, dens_fake$y)))
      
      plot(
        dens_real,
        xlim = xlims,
        ylim = ylims,
        yaxt = "n",
        ylab = "",
        lwd = 2,
        col = viridis::viridis(2, 0.8)[1],
        main = paste0("Column: ", x),
        bty = "n"
      )
      lines(dens_fake,
            col = viridis::viridis(3, 0.8)[2],
            lwd = 2)
    })
    plot(
      0,
      0,
      type = "n",
      ylab = "",
      xlab = "",
      main = "",
      bty = "n",
      yaxt = "n",
      xaxt = "n"
    )
    legend(
      "center",
      legend = c("Real Data", "Synthetic Data"),
      border = NA,
      lwd = 2,
      col = viridis::viridis(3, 0.8)[1:2],
      lty = "solid",
      bty = "n"
    )
    mtext(paste0(trial_name, "\nDensities of Real and Synthetic Data"), font = 2, outer = TRUE)
    if(save_plots){
      dev.off()
    }
  }


show_real_synth <-
  function(synth_data,
           real_data,
           trial_name = NULL,
           folder = NULL,
           save_plots = FALSE) {
    if (is.null(trial_name)) {
      trial_name <- paste0(round(as.numeric(Sys.time()), 0), "_synthetic_data_trial")
    }
    
    
    cat("\nReal Means: ", colMeans(real_data))
    cat("\nSynthetic Means: ", colMeans(synth_data))
    cat("\nReal Correlations\n")
    print(cor(real_data))
    cat("\nSynthetic Correlations\n")
    print(cor(synth_data))
    
    if (save_plots) {
      pdf(
        file = paste0(
          folder,
          ifelse(is.null(folder), "", "/"),
          trial_name,
          "_correlations.pdf"
        ),
        width = 16,
        height = 9
      )
    }
    par(mfrow = c(1, 2), oma = c(0, 0, 5, 0))
    corrplot::corrplot(cor(real_data), tl.col = "black", method = "color",addCoef.col = "black", col = viridis::viridis(200), main = "Real Data", number.cex = 0.5, number.font = 1, cex.main = 0.8, mar = c(0,0,1,0))
    corrplot::corrplot(cor(synth_data), tl.col = "black", method = "color",addCoef.col = "black", col = viridis::viridis(200), main = "Synthetic Data", number.cex = 0.5, number.font = 1, cex.main = 0.8, mar = c(0,0,1,0))
    mtext(paste0(trial_name, "\nCorrelations"), outer = TRUE, font = 2)
    if(save_plots){
      dev.off() 
    }
    if (save_plots) {
      pdf(
        file = paste0(
          folder,
          ifelse(is.null(folder), "", "/"),
          trial_name,
          "_distributions.pdf"
        ),
        width = 16,
        height = 16
      )
    }
    
    if(ncol(real_data) < 12){
      par(mfrow = c(4, 3), oma = c(0, 0, 4, 0))
    }
    if(ncol(real_data) >= 12 & ncol(real_data) < 16){
      par(mfrow = c(4, 4))
    }
    if(ncol(real_data) >= 16 & ncol(real_data) < 20){
      par(mfrow = c(5, 4))
    }
    if(ncol(real_data) >= 20 & ncol(real_data) < 25){
      par(mfrow = c(5, 5))
    }
    
    sapply(1:ncol(real_data), function(x) {
      dens_real <- density(real_data[, x])
      dens_fake <- density(synth_data[, x])
      
      xlims <-
        c(min(c(dens_real$x, dens_fake$x)), max(c(dens_real$x, dens_fake$x)))
      ylims <-
        c(min(c(dens_real$y, dens_fake$y)), max(c(dens_real$y, dens_fake$y)))
      
      plot(
        dens_real,
        xlim = xlims,
        ylim = ylims,
        yaxt = "n",
        ylab = "",
        lwd = 2,
        col = viridis::viridis(2, 0.8)[1],
        main = paste0("Column: ", x),
        bty = "n"
      )
      lines(dens_fake,
            col = viridis::viridis(3, 0.8)[2],
            lwd = 2)
    })
    plot(
      0,
      0,
      type = "n",
      ylab = "",
      xlab = "",
      main = "",
      bty = "n",
      yaxt = "n",
      xaxt = "n"
    )
    legend(
      "center",
      legend = c("Real Data", "Synthetic Data"),
      border = NA,
      lwd = 2,
      col = viridis::viridis(3, 0.8)[1:2],
      lty = "solid",
      bty = "n"
    )
    mtext(paste0(trial_name, "\nDensities of Real and Synthetic Data"), font = 2, outer = TRUE)
    if(save_plots){
      dev.off()
    }
  }


sample_synthetic_data <-
  function(res, n = 5000, sample_noise, device) {
    z <-
      sample_noise(n,
                   embedding_dim = attrs$embedding_dim,
                   device = device)
    
    synth_data <- as_array(res$generator(z)$detach()$cpu())
    
    return(synth_data)
  }


data_transformer <- R6::R6Class(
  "data_transformer",
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Create a new data_transformer object
    initialize = function() {
      NULL
    },
    
    fit_continuous = function(column = NULL, data = NULL) {
      data <- data[, 1]
      mean <- mean(data, na.rm = TRUE)
      std <- sd(data, na.rm = TRUE)
      
      return(
        list(
          name = column,
          z_transform = NULL,
          mean = mean,
          std = std,
          output_info = list(1, "linear"),
          output_dimensions = 1
        )
      )
    },
    fit_discrete = function(column = NULL, data = NULL) {
      column <- column
      data <- factor(data[, 1])
      levs <- levels(data)
      categories <- length(levs)
      
      return(list(
        name = column,
        levs = levs,
        output_info = list(categories, "softmax"),
        output_dimensions = categories
      ))
      
    },
    #' @description
    #' Fit a data_transformer to data.
    #' @param data The data set to transform
    #' @param discrete_columns Column ids for columns with discrete/nominal values to be one hot encoded.
    #' @examples
    #' data <- sample_toydata()
    #' transformer <- data_transformer$new()
    #' transformer$fit(data)
    fit = function(data, discrete_columns = NULL) {
      self$output_info <- list()
      self$output_dimensions <- 0
      
      self$meta <- list()
      
      if (is.null(colnames(data))) {
        col_ids <- 1:ncol(data)
      } else {
        col_ids <- colnames(data)
      }
      
      for (column in col_ids) {
        column_data <- data[, which(column == col_ids), drop = F]
        if (column %in% discrete_columns) {
          meta <- self$fit_discrete(column, column_data)
        } else {
          meta <- self$fit_continuous(column, column_data)
        }
        self$output_info[[length(self$output_info) + 1]] <-
          meta$output_info
        self$ouput_dimensions <-
          self$output_dimensions + meta$output_dimensions
        self$meta[[length(self$meta) + 1]] <- meta
      }
      invisible(self)
    },
    transform_continuous = function(column_meta, data) {
      mean <- column_meta$mean
      std <- column_meta$std
      
      z <- (data - mean) / std
      
      return(z)
    },
    transform_discrete = function(column_meta, data) {
      oh <- model.matrix( ~ 0 + factor(data, levels = column_meta$levs))
      colnames(oh) <- column_meta$levs
      oh_na <- array(NA, dim = c(length(data), ncol(oh)))
      oh_na[!is.na(data), ] <- oh
      colnames(oh_na) <- colnames(oh)
      return(oh_na)
    },
    #' @description
    #' Transform data using a fitted data_transformer. (From original format to transformed format.)
    #' @param data The data set to transform
    #' @examples
    #' data <- sample_toydata()
    #' transformer <- data_transformer$new()
    #' transformer$fit(data)
    #' transformed_data <- transformer$transform(data)
    transform = function(data) {
      values <- list()
      for (meta in self$meta) {
        column_data <- data[, meta$name]
        if ("levs" %in% names(meta)) {
          values[[length(values) + 1]] <-
            self$transform_discrete(meta, column_data)
        } else {
          values[[length(values) + 1]] <-
            self$transform_continuous(meta, column_data)
        }
      }
      
      return(do.call(cbind, values))
      
    },
    inverse_transform_continuous = function(meta, data) {
      mean <- meta$mean
      std <- meta$std
      
      u <- data
      
      column <- u * std + mean
      
      return(column)
    },
    inverse_transform_discrete = function(meta, data) {
      levs <- meta$levs
      #column <- factor(round(data) %*% 1:length(levs))
      #column <- factor(t(apply(data, 1, function(x){
      #ranks <- rank(x, ties.method = "random")
      #ranks == max(ranks)
      #})*1) %*% 1:length(levs))
      max_index <- max.col(data, ties.method = "random")
      row_col_index <-
        stack(setNames(max_index, seq_along(max_index)))
      max_matrix <-
        Matrix::sparseMatrix(
          as.numeric(row_col_index[, 2]),
          row_col_index[, 1],
          x = 1,
          dims = c(max(as.numeric(row_col_index[, 2])), length(levs))
        )
      
      column <- factor(as.matrix(max_matrix) %*% 1:length(levs))
      levels(column) <- levs
      column <- as.numeric(as.character(column))
      return(column)
    },
    #' @description
    #' Inverse Transform data using a fitted data_transformer. (From transformed format to original format.)
    #' @param data The data set to transform
    #' @examples
    #' data <- sample_toydata()
    #' transformer <- data_transformer$new()
    #' transformer$fit(data)
    #' transformed_data <- transformer$transform(data)
    #' reconstructed_data <- transformer$inverse_transform(transformed_data)
    inverse_transform = function(data) {
      start <- 1
      output <- list()
      column_names <- list()
      for (meta in self$meta) {
        dimensions <- meta$output_dimensions
        columns_data <- data[, start:(start + dimensions - 1)]
        
        if ("levs" %in% names(meta)) {
          inverted <- self$inverse_transform_discrete(meta, columns_data)
        } else {
          inverted <- self$inverse_transform_continuous(meta, columns_data)
        }
        output[[length(output) + 1]] <- inverted
        column_names[[length(column_names) + 1]] <- meta$name
        start <- start + dimensions
      }
      output <- do.call("cbind", output)
      colnames(output) <- do.call("c", column_names)
      
      return(output)
    }
  )
)

invert_generator <- function(data, encoder_arch, generator, data_dim, noise_dim, hidden_units = list(128, 128, 128), n_steps = 1000, e_lr = 0.001, device) {
  
  encoder <- encoder_arch(noise_dim = data_dim, data_dim = noise_dim, hidden_units = hidden_units)$to(device = device)
  e_optim <-
    torch::optim_adam(
      encoder$parameters,
      lr = e_lr
    )
  loss <- torch::nn_mse_loss()
  e_losses <- NULL
  
  for(i in 1:n_steps){
    tmp_data <- torch::torch_tensor(data)
    encoding <- encoder(tmp_data)
    re_con <- generator(encoding)
    
    e_loss <- loss(tmp_data, re_con)
    
    e_losses <- c(e_losses, as_array(e_loss))
    
    encoder$zero_grad()
    e_loss$backward()
    e_optim$step()
  }
  
  return(list(encoder = encoder,
              losses = e_losses))
  
}


RNGkind("L'Ecuyer-CMRG")
set.seed(230925)

# Set up data
n <- 5000

data <- NULL

for (i in 1:5) {
  data <- cbind(data, round(rnorm(n, 100, 20)))
}
for (i in 6:7) {
  data <-
    cbind(data, round(c(
      rnorm(n * 0.3, 100, 10), rnorm(n * 0.7, 50, 15)
    )))
}
for (i in 8:10) {
  data <-
    cbind(data, round(c(
      rnorm(n * 0.3, 50, 15), rnorm(n * 0.7, 100, 10)
    )))
}

# Normalize data to facilitate GAN training
transformer <- data_transformer$new()
transformer$fit(data)
transformed_data <- transformer$transform(data)

# Run a GAN with the CTGAN settings
set.seed(230925)

res_CTGAN <-
  run_experiment(
    mb_size = 500,
    n_steps = 3000, # 1 epoch is the number of steps it takes to see the entire dataset
    real_data = transformed_data,
    data_dim = 10,
    embedding_dim = 128,
    g_hidden_units = list(256, 256),
    d_hidden_units = list(256, 256),
    adam_betas = c(0.5, 0.9),
    d_lr = 2e-4,
    g_lr = 2e-4,
    d_decay = 1e-6,
    g_decay = 1e-6,
    GAN_value_fct = WGAN_value_fct,
    d_sigmoid = FALSE,
    pac = 10,
    CTGAN = TRUE
  )

show_results(
  res_CTGAN,
  real_data = transformed_data,
  sample_noise = sample_normal,
  device = device,
  folder = "figures",
  trial_name = "CTGAN",
  save_plots = TRUE
)

# Invert the Generator to get the latent encodings of the training data

inverted_generator_CTGAN <-
  invert_generator(
    transformed_data,
    Generator,
    res_CTGAN$generator,
    data_dim = 10,
    noise_dim = 128,
    device = device
  )

# Check if the Inversion worked
# plot(1:1000, inverted_generator_CTGAN$losses, type = "l")

# Generate "synthetic" data based on latent codes of real data

# Get latent codes
z_data <-
  as_array(inverted_generator_CTGAN$encoder(torch_tensor(transformed_data)))

# Generate synthetic data
show_real_synth(as_array(res_CTGAN$generator(z_data)),
                transformed_data,
                folder = "figures",
                save_plots = TRUE)


# Try with noisy latent codes
z_data_noise <-
  z_data + array(rnorm(prod(dim(z_data)), 0, 0.5), dim = dim(z_data))

show_real_synth(as_array(res_CTGAN$generator(z_data_noise)),
                transformed_data,
                folder = "figures",
                save_plots = TRUE)


# A better GAN
# Changes to CTGAN:
# - Discriminator Architecture
# - Generator depth and width (now 3 layers with 128 neurons each, vs. 2 layers with 256 each)
# - Optimizer/Adam Hyperparameters
# - Loss Function, now relativistic Least Square loss

set.seed(230925)
res_CTRGAN_LS_3_128_full <-
  run_experiment(
    n_steps = 3000,
    real_data = transformed_data,
    data_dim = 10,
    embedding_dim = 128,
    g_arch = CTGAN_Generator,
    g_hidden_units = list(128, 128, 128),
    d_hidden_units = list(128, 128, 128),
    adam_betas = c(0.9, 0.999),
    d_lr = 0.001,
    g_lr = 0.001,
    d_decay = 0,
    g_decay = 0,
    GAN_value_fct = rel_avg_LSGAN_value_fct,
    d_sigmoid = FALSE,
    pac = 10,
    CTGAN = FALSE
  )

# show_results(
#   res_CTRGAN_LS_3_128_full,
#   real_data = transformed_data,
#   sample_noise = sample_normal,
#   device = device
# )

show_results(
  res_CTRGAN_LS_3_128_full,
  real_data = transformed_data,
  sample_noise = sample_normal,
  device = device,
  folder = "figures",
  trial_name = "Relativistic CTGAN LS 3 128 Full Batch",
  save_plots = TRUE
)


inverted_generator_CTRGAN <-
  invert_generator(
    transformed_data,
    Generator,
    res_CTRGAN_LS_3_128_full$generator,
    data_dim = 10,
    noise_dim = 128,
    device = device
  )

plot(1:1000, inverted_generator_CTRGAN$losses, type = "l")

z_data <-
  as_array(inverted_generator_CTRGAN$encoder(torch_tensor(transformed_data)))
show_real_synth(as_array(res_CTRGAN_LS_3_128_full$generator(z_data)),
                transformed_data,
                folder = "figures",
                save_plots = TRUE)

z_data_noise <-
  z_data + array(rnorm(prod(dim(z_data)), 0, 0.1), dim = dim(z_data))

show_real_synth(as_array(res_CTRGAN_LS_3_128_full$generator(z_data_noise)),
                transformed_data,
                save_plots = F)


transformed_data - as_array(res_CTRGAN_LS_3_128_full$generator(z_data_noise))
transformed_data - as_array(res_CTRGAN_LS_3_128_full$generator(z_data))



# independent z
z <- array(rnorm(prod(dim(z_data)), 0, 1), dim = dim(z_data))

synthetic_data <- as_array(res_CTRGAN_LS_3_128_full$generator(z))
show_real_synth(synthetic_data,
                transformed_data,
                save_plots = F)


reconstructed_data <- as_array(res_CTRGAN_LS_3_128_full$generator(z_data))
noisy_reconstructed_data <- as_array(res_CTRGAN_LS_3_128_full$generator(z_data_noise))

# Calculate euclidean distances between synthetic and real data points

calculate_distance <- function(data, synthetic_data) {
  res <- NULL 
  
  for(i in 1:dim(synthetic_data)[1]) {
    
    tmp_dist <- as.matrix(dist(rbind(data, synthetic_data[i,])))[dim(data)[1] + 1,1:dim(data)[1]]
    tmp_id <- unname(which.min(tmp_dist))
    tmp_min_dist <- unname(tmp_dist[tmp_id])
    
    res <- rbind(res, cbind(tmp_id, tmp_min_dist))
    cat(i, "\n")
  }
  
  return(res)
}



synthpop_df <- synthpop::syn(transformed_data, cart.minbucket = 5)$syn
synthpop_dist <- calculate_distance(transformed_data, synthpop_df[1:10,])

calculate_distance(transformed_data, noisy_reconstructed_data[1:10,])

View(rbind(transformed_data[synthpop_dist[,1],], synthpop_df[1:10,]))