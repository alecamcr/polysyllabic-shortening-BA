# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""


import pandas as pd
import os

# path of the file containing the data. the files to read must be csv files.
data = '/Users/alejandracamelocruz/Desktop/Tesis/data'

#-------------------------------- functions--------------------------------

# complete csv original tables
def get_tables(x, ending = ''):
    output = {}
    for root, subdirs, files in os.walk(x):
        for file in files:
            path = os.path.join(root, file)
            if file.endswith(ending):
                output[os.path.basename(os.path.dirname(path))] = pd.read_csv(path)
    return output




#function to retriever only ph column with phones and get it as a list to put it in the sounds dictinary
def sound_list(diccionario_input):
    diccionario_output = {}
    for elemento in diccionario_input:
        diccionario_output[elemento] =  diccionario_input[elemento].ph.to_list()
    return diccionario_output

def start_list(diccionario_input):
    diccionario_output = {}
    for elemento in diccionario_input:
        diccionario_output[elemento] =  diccionario_input[elemento].start.to_list()
    return diccionario_output

def end_list(diccionario_input):
    diccionario_output = {}
    for elemento in diccionario_input:
        diccionario_output[elemento] =  diccionario_input[elemento].end.to_list()
    return diccionario_output

def vowel_list(diccionario_input):
    diccionario_output = {}
    for elemento in diccionario_input:
        diccionario_output[elemento] =  diccionario_input[elemento].vowels.to_list()
    return diccionario_output

def word_list(diccionario_input):
    diccionario_output = {}
    for elemento in diccionario_input:
        diccionario_output[elemento] =  diccionario_input[elemento].wd.to_list()
    return diccionario_output


#---------------------- creating lists with information from csv files - data frames------------------------------

csv_complete = get_tables(data, 'ph.csv')
sounds = sound_list(csv_complete)
start_original = start_list(csv_complete)
end_original = end_list(csv_complete)
words = word_list(csv_complete)

csv_sampa = get_tables(data, 'conventions.csv')
vowels = vowel_list(csv_sampa)


#---------------------- Main program------------------------------:







def syllable_counting():
    numbers = {}
    for language in words:
        numbers[language] = []
    for language, lista in words.items():
        rango = len(lista)
        number = 0
        repetition = 0
        for index in range(rango):
            try:
                word = lista[index]
                previous_word = lista[index-1]
                next_word = lista[index+1]
                sound = sounds[language][index]
                if sound in vowels[language]:
                    number = number + 1 
                if '<' in word:
                    numbers[language].append(word)
                elif word != previous_word and word != next_word:
                    numbers[language].append(1)
                    number = 0
                elif word != previous_word and word == next_word:
                    repetition = repetition + 1
                elif word == previous_word and word == next_word:
                    repetition = repetition + 1
                elif word == previous_word and word != next_word:
                    repetition = repetition + 1
                    for _ in range(repetition):
                        numbers[language].append(number)
                    number = 0
                    repetition = 0
                        
                
                    
            except IndexError:
                numbers[language].append(word)
            except TypeError:
                numbers[language].append('error')
                print('error for syllable number')
                print(language, index, word, type(word), sound, type(sound))
                
    return numbers



def syllable_position ():
    syllables = {}
    positions = {}
    for language in words:
        syllables[language] = []
        positions[language] = []
    
    for language in sounds.keys():
        syllable = ''
        position = ''
        count = 0
        last_count = 0
        lista_sounds = sounds[language]
        lista_palabra = words[language]
        rango = len(lista_sounds)
        for index in range(rango):
            try:
                sound = lista_sounds[index]
                previous = lista_sounds[index-1]
                next1 = lista_sounds[index+1]
                
                current_word = lista_palabra[index]
                previous_word = lista_palabra[index-1]
                next_word = lista_palabra[index+1]
                if '<' in sound:
                    syllables[language].append(sound)
                    positions[language].append(sound)
                    syllable = ''
                    count = 0
                    last_position = ''
                    last_count = 0
                    last_syllable = ''
                    
    #for arapaho cases of isolated consonants. Syllabic consonants?
    
                elif '<' in previous and '<' in next1:
                    syllables[language].append(sound)
                    positions[language].append('monosyllable')
                    syllable = ''
                    count = 0
                    
                elif sound in vowels[language]:
                    count = count + 1
                    syllable = syllable + sound
                    
                    
    #---------------------------position--------------------------------
    
    #discriminating between word final and utterance final. 
    # this somehow only accounts when before the syllable there is no consonants
                    if position == 'utterance initial':
                        position = 'utterance initial'
                    elif position == 'word initial':
                        position = 'word initial'
                    elif position == 'special start':
                        position = 'special start'
                    # elif position == 'monosyllable':
                    #     position = 'monosyllable'
                    elif '<' in previous:
                        if previous == '<p:>':
                            position = 'utterance initial'
                        else:
                            position = 'special start'
                    elif '<' in next1:
                        if next1 == '<p:>':
                            position = 'utterance final'
                        else:
                            position = 'special end'
                    elif current_word != previous_word and current_word == next_word:
                        position = 'word initial'
                    elif current_word != next_word and current_word == previous_word:
                        position = 'word final'
                    # elif current_word != next_word and current_word != previous_word:
                    #     position = 'monosyllable'
                    else:
                        position = 'medial'
    #------------------------------------------------------------
    
                    for x in range(count):
                        syllables[language].append(syllable)
    #-------------------------------------------------------------
                        positions[language].append(position)
                    last_count = count
                    last_syllable = syllable
                    last_position = position
                    position = ''
                    syllable = ''
                    count = 0
            
    #case in which a word ends with a syllable:
    
                elif current_word != next_word:
                    syllable = last_syllable + syllable + sound
                    count = last_count + count + 1
    #-----------------------------------------------------------
                    if '<' in next1:
                        if next1 == '<p:>':
                            position = 'utterance final'
                        else:
                            position = 'special end'
                    elif current_word != next_word:
                        position = 'word final'
                    else:
                        position = last_position
                    if '<' in next1:
                        if next1 == '<p:>':
                            position = 'utterance final'
                        else:
                            position = 'special end'
    
    #------------------------------------------------------------
                    for x in range(last_count):
                        syllables[language].pop()
    #------------------------------------------------------------
                        positions[language].pop()
    #------------------------------------------------------------
                    for x in range(count):
                        syllables[language].append(syllable)
    #-----------------------------------------------------------
                        positions[language].append(position)
    # -----------------------------------------------------------
                    last_count = count
                    last_syllable = ''
                    last_position = ''
    
                    syllable = ''
                    last_syllable = ''
                    count = 0
                    
                else:
                    syllable = syllable + sound
                    count = count + 1
                    if count == 1 and '<' in previous:
                        if previous == '<p:>':
                            position = 'utterance initial'
                        else:
                            position = 'special start'
                    elif count == 1 and current_word != previous_word:
                        position = 'word initial'

                        
            except IndexError:
                syllables[language].append(sound)
                positions[language].append(sound)
                
    return syllables, positions

# test for syllable errors
# lista = sounds['arapaho']
# length = len(lista)
# for index in range(length):
#     sonido = lista[index]
#     silaba = syllables['arapaho'][index]
#     if sonido not in silaba:
#         print(index, sonido, silaba)
#         break

def utterance_syllable_counting():
    numbers_utterance = {}
    for language in words:
        numbers_utterance[language] = []
    for language, lista in syllables.items():
        rango = len(lista)
        utterance_number = 0
        for index in range(rango):
            try:
                syllable = lista[index]
                next_syllable = lista[index+1]
                utterance_number = utterance_number + 1
                if '<' in syllable:
                    numbers_utterance[language].append(syllable)
                if '<' in next_syllable:
                    for _ in range(utterance_number-1):
                        numbers_utterance[language].append(utterance_number-1)
                    utterance_number = 0
                        
                
                    
            except IndexError:
                numbers_utterance[language].append(syllable)
            except TypeError:
                numbers_utterance[language].append(syllable)
                print('error for syllable number')
                print(language, index, syllable, type(syllable))
                
    return numbers_utterance


            
def syllable_durations_count ():
    durations = {}
    for language in words:
        durations[language] = []
    for language in sounds.keys():
        list_sounds = sounds[language]
        list_words = words[language]
        rango = len(list_sounds)
        for index in range(rango):
            try:
                sound = list_sounds[index]
                previous = list_sounds[index-1]
                next1 = list_sounds[index+1]
                
                current_word = list_words[index]
                next_word = list_words[index+1]
                
                if '<' in sound:
                    durations[language].append(sound)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_initial = 0
                    
                
                elif '<' in previous and '<' in next1:
                    initial_time = start_original[language][index]
                    final_time = end_original[language][index]
                    duration = final_time - initial_time
                    durations[language].append(duration)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_initial = 0
                    
                elif sound in vowels[language]:
                    count = count + 1
                    if count == 1:
                        initial_time = start_original[language][index]
                    final_time = end_original[language][index]
                    duration = final_time - initial_time
                    for x in range(count):
                        durations[language].append(duration)
                    last_count = count
                    last_initial = initial_time
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0

                elif current_word != next_word:
                    count = last_count + count + 1
                    if count ==  1:
                        initial_time = start_original[language][index]
                    else:
                        initial_time = last_initial
                    final_time = end_original[language][index]
                    duration = final_time - initial_time
                    for x in range(last_count):
                        durations[language].pop()
                    for x in range(count):
                        durations[language].append(duration)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_initial = 0
                    
                else:
                    count = count + 1
                    if count == 1:
                        initial_time = start_original[language][index]
                        
                        
            except IndexError:
                durations[language].append(sound)
                
    return durations


def vowel_durations_count ():
    durations = {}
    for language in words:
        durations[language] = []
    for language in sounds.keys():
        count = 0
        initial_time = None
        list_sounds = sounds[language]
        list_words = words[language]
        rango = len(list_sounds)
        for index in range(rango):
            try:
                sound = list_sounds[index]
                previous = list_sounds[index-1]
                next1 = list_sounds[index+1]
                
                current_word = list_words[index]
                next_word = list_words[index+1]
                
                if '<' in sound:
                    durations[language].append(sound)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_duration = 0
                    
                
                elif '<' in previous and '<' in next1:
                    initial_time = start_original[language][index]
                    final_time = end_original[language][index]
                    duration = final_time - initial_time
                    durations[language].append(duration)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_duration = 0
                    
                elif sound in vowels[language]:
                    count = count + 1
                    initial_time = start_original[language][index]
                    final_time = end_original[language][index]
                    duration = final_time - initial_time
                    for x in range(count):
                        durations[language].append(duration)
                    last_count = count
                    last_duration = duration
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0

                elif current_word != next_word:
                    count = last_count + count + 1
                    duration = last_duration
                    for x in range(last_count):
                        durations[language].pop()
                    for x in range(count):
                        durations[language].append(duration)
                    count = 0
                    initial_time = 0
                    final_time = 0
                    duration = 0
                    last_count = 0
                    last_duration = 0
                    
                else:
                    count = count + 1
                        
                        
            except IndexError:
                durations[language].append(sound)
                
    return durations

        

numbers = syllable_counting()
syllables, positions = syllable_position()
syllable_durations = syllable_durations_count()
vowel_durations = vowel_durations_count()
utterance_numbers = utterance_syllable_counting()

#creating monosyllables
for language in numbers:
    sil_nums = numbers[language]
    for index in range(len(sil_nums)):
        number = sil_nums[index]
        if number == 1:
            positions[language][index] = 'monosyllable'
    
    
for key, value in syllables.items():
    csv_complete[key].insert(6, column='syllable', value = value)

for key, value in positions.items():
    csv_complete[key].insert(7, column='position', value = value)
    
for key, value in syllable_durations.items():
    csv_complete[key].insert(8, column='syllable_duration', value = value)
    
for key, value in vowel_durations.items():
    csv_complete[key].insert(9, column='vowel_duration', value = value)

for key, value in numbers.items():
    csv_complete[key].insert(16, column='number', value = value)

for key, value in utterance_numbers.items():
    csv_complete[key].insert(18, column='number_in_utterance', value = value)
    

#deleting no processed row yurakare
csv_complete['yurakare'] = csv_complete['yurakare'].drop([116600])

for language in csv_complete.keys():
    ##this line drops all the information containing not relevant linguistic information 
    csv_complete[language] = csv_complete[language][csv_complete[language]['wd'].str.contains('<') == False]
    csv_complete[language] = csv_complete[language][csv_complete[language].number > 1]
    ##here I wanted to filter until 8 syllables. Now I want to get all the data 
    #csv_complete[language] = csv_complete[language][csv_complete[language].number <= 8]
    csv_complete[language] = csv_complete[language].drop(columns=['ph_ID', 'ref', 'tx', 'ft', 'wd_ID', 'mb_ID', 'ps', 'gl'])


for key, value in csv_complete.items():
    csv_complete[key].to_csv('new_data_{}.csv'.format(key))    
    