import pandas as pd
import re
import os
import numpy as np
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer

def preprocess(text, remove_stop_words = False, stem=True):
    # remove link
    text = re.sub(r"(http?\://|https?\://|www)\S+", " ", str(text).lower()).strip()
    # remove newlines
    text = re.sub(r'\n', ' ', text)
    # remove puctuations and special characters
    text = re.sub(r'\W+', ' ', text)
    # Substituting multiple spaces with single space
    text = re.sub(r'\s+', ' ', text, flags=re.I)
    # remove first space
    text = re.sub(r'^\s+', '', text)
    # Removing prefixed 'b'
    text = re.sub(r'^b\s+', '', text)
    
    if remove_stop_words or stem:
        tokens = []
        for token in text.split():
            if remove_stop_words:
                if token not in stop_words:
                    if stem:
                        tokens.append(stemmer.stem(token))
                    else:
                        tokens.append(token)
            else:
                if stem:
                    tokens.append(stemmer.stem(token))
                else:
                    tokens.append(token)
        return(" ".join(tokens))
    else:
        return(text)

def get_embedding_matrix(method = "word2vec", has_model=False, embedding_dim=200, save_vectors=False, path=None):
    if method == "word2vec":
        if not has_model:
            w2v_model = gensim.models.word2vec.Word2Vec(size=embedding_dim, 
                                                    window=7, 
                                                    min_count=10, 
                                                    workers=8)
            w2v_model.build_vocab(documents)
            words = w2v_model.wv.vocab.keys()
            w2v_model.train(documents, total_examples=len(documents), epochs=32)
            if save_vectors:
                w2v_model.save(path)
        
        embedding_matrix = np.zeros((vocab_size, embedding_dim)) #Return a new array of given shape and type, filled with zeros.
        for word, i in tokenizer.word_index.items():
            if word in w2v_model.wv:
                embedding_matrix[i] = w2v_model.wv[word]
    
    if method == "glove":
        #https://www.kaggle.com/hamishdickson/bidirectional-lstm-in-keras-with-glove-embeddings
        embeddings_index = {}
        f = open(path, encoding='UTF-8')
        for line in f:
            values = line.split()
            word = values[0] ## The first entry is the word
            coefs = np.asarray(values[1:], dtype='float32') ## These are the vecotrs representing the embedding for the word
            embeddings_index[word] = coefs
        f.close()

        #max_features = 50000
        max_features = vocab_size - 1
        num_words = min(max_features, vocab_size - 1) + 1
        #print(num_words)

        # first create a matrix of zeros, this is our embedding matrix
        embedding_matrix = np.zeros((num_words, embedding_dim))

        # for each word in out tokenizer lets try to find that work in our w2v model
        for word, i in tokenizer.word_index.items():
            if i > max_features:
                continue
            embedding_vector = embeddings_index.get(word)
            if embedding_vector is not None:
                # we found the word - add that words vector to the matrix
                embedding_matrix[i] = embedding_vector
            #else:
                # doesn't exist, assign a random vector
            #    embedding_matrix[i] = np.random.randn(embedding_dim)
    
    return(embedding_matrix)

def get_w2v_model(embedding_dim = 200, window_size=5, save_vectors = False, path = None):
    w2v_model = gensim.models.word2vec.Word2Vec(size=embedding_dim, 
                                                window=window_size, 
                                                min_count=10, 
                                                workers=8)
    w2v_model.build_vocab(documents)
    words = w2v_model.wv.vocab.keys()
    w2v_model.train(documents, total_examples=len(documents), epochs=64)
    if save_vectors:
        w2v_model.save(path)
        
    return(w2v_model)
