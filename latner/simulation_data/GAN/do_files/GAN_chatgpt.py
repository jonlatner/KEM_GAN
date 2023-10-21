import numpy as npimport tensorflow as tffrom tensorflow.keras.layers import Dense, Inputfrom tensorflow.keras.models import Modelfrom tensorflow.keras.optimizers import Adam# Generate Original Dataoriginal_data = np.random.normal(0, 1, (10000, 1))# Generator Modeldef build_generator():    model = tf.keras.Sequential([        Dense(20, activation='relu', input_dim=10),        Dense(1, activation='linear')    ])    return model# Discriminator Modeldef build_discriminator():    model = tf.keras.Sequential([        Dense(20, activation='relu', input_dim=1),        Dense(1, activation='sigmoid')    ])    return model# Build and compile Discriminatordiscriminator = build_discriminator()discriminator.compile(loss='binary_crossentropy', optimizer=Adam(), metrics=['accuracy'])# Build Generatorgenerator = build_generator()z = Input(shape=(10,))sample = generator(z)# For the combined model, we will only train the generatordiscriminator.trainable = Falsevalidity = discriminator(sample)combined = Model(z, validity)combined.compile(loss='binary_crossentropy', optimizer=Adam())# Train GANdef train(epochs, batch_size=128):    valid = np.ones((batch_size, 1))    fake = np.zeros((batch_size, 1))        for epoch in range(epochs):                # Train Discriminator        idx = np.random.randint(0, original_data.shape[0], batch_size)        real_samples = original_data[idx]                noise = np.random.normal(0, 1, (batch_size, 10))        gen_samples = generator.predict(noise)                d_loss_real = discriminator.train_on_batch(real_samples, valid)        d_loss_fake = discriminator.train_on_batch(gen_samples, fake)        d_loss = 0.5 * np.add(d_loss_real, d_loss_fake)                # Train Generator        noise = np.random.normal(0, 1, (batch_size, 10))        g_loss = combined.train_on_batch(noise, valid)                print(f"{epoch}/{epochs} [D loss: {d_loss[0]} | D Accuracy: {100 * d_loss[1]}] [G Loss: {g_loss}]")train(epochs=300)# Generate Synthetic Datanum_samples = 1000noise = np.random.normal(0, 1, (num_samples, 10))synthetic_data = generator.predict(noise)