# AFTOVolio

Author and [software
developer:](https://hackage.haskell.org/package/aftovolio-ukrainian)
Oleksandr Zhabenko

# Introduction

Here is a brief introduction of AFTOVolio ideas and usage (that is Dutch 'Aanpak van Fonetische Talen voor het Ontdekken van de VOorkeursLIjnOpties' (Dutch people consider ij as a letter and consequently one sound, but we use just first one for good readability) — 'Phonetic Languages Approach for Discovering the Preferred Line Options').

Using AFTOVolio lets a person learn how to write the texts with the desired properties in pronunciation and prosody. 

The idea behind AFTOVolio is that the text is first created based on the meaning of what needs to be said. And then (or even during this stage) it is organised (edited) into a better pronunciation, actually prosody, including a more rhythmic one.

## Basic idea

There are different languages. They have different structure and rules. It is possible to create and use (based on one of existing widely used and well-spoken languages, in particular Ukrainian in this work) a 'phonetic' language that is better suited for poetry and music. It is even possible to create different versions of phonetic language. This project proposes to create several different phonetic languages based on Ukrainian and gives some templates to use these ideas also for many other languages.

## What does this mean?

If someone builds a phrase in a language that violates the rules of grammar or semantics, then this error is visible to a skilled speaker at once, it is identified as such almost instantly. Instead, if the sound of a phrase has some phonetic features, not counting accents, for example, the complexity of pronunciation or vice versa lightness, smoothness or abruptness, etc., then it is possible to identify it as an error or something significant not immediately or with special attention. One can imagine this as giving preference to the language semantics (meaning) and grammar, but less weight to phonetics. Phonetic language is that one built specifically to enhance the meaning and importance of the phonetic component itself.

## Phonetic or prosodic language?

An interesting question is whether to call the approach of \"phonetic\" or \"prosodic\" language. But I must say that we study the actual phonetic features, what is associated with the sound of speech. Among them is that which concerns certain phonetic phenomenae in the general case, in particular phonemes or even palatalization. These questions are generally not the subject of the study of prosody as a science, as a certain part of phonetics, but are the subject of a broader study of phonetics. Moreover, there are no restrictions and bindings of the proposed approach to the actual syllables, which is more typical for the subject of the study of prosody. Generalizations can be made for more general cases.

However, at this stage of development, the vast majority of information here relates to or is directly related to syllables and prosody. Therefore, I leave the name \"phonetic languages\", given that prosody is a more specific branch of phonetics.

## Ethical component

The proposed approach is similar to the approach of music theory. Thus, in music, among all sounds, musical ones stand out, later consonances and dissonances are studied, later notes, intervals, chords, melodies, composition, etc. There are recommendations, but they do not bind the creators, but help. Similarly, the proposed approach is designed to provide such assistance. Its strangeness at first glance cannot be a reason to deny it.

For Christians, to whom the author himself belongs, the words of Moses are important: \"And Moses went out, and spake unto the people the word of the LORD, and gathered together seventy men of the elders of the people, and set them near the tabernacle. And the LORD came down in the cloud, and spake unto him, Two of the men remained in the state, one named Eldad and the other named Modad, but the Spirit also rested on them. And the young man ran, and told Moses, and said, Eldad and Modad are prophesying in the camp: And Joshua the son of Nun, the servant of Moses, one of his chosen ones, answered and said, My lord Moses. But Moses said unto him, Hast thou not jealous of me? O that all the people of the LORD should be prophets, if the LORD would send His Spirit upon them\". (Numbers XI, 24-29).

It is good that everybody can well write and speak.

## First idea

Imagine that you can understand the information in the text regardless of the order of the words and preserve only the most necessary grammar (for example, the rule does not separate the preposition and the next word is preserved). Understand just like reading a text (after some learning and training, perhaps), in which only the first and last letters are preserved in words in their positions, and all the others are mutually mixed with each other. So imagine that you can understand (and express your thoughts, feelings, motives, etc.) the message of the text without adherence to strict word order. In this case, you can organize the words (keeping the most necessary grammar to reduce or eliminate possible ambiguity due to grammar, or rather a decrease in its volume), placing them so that they provide a more interesting phonetic sounding. You can try to create poetic (or at least a little more rhythmic and expressive) text or music.

It can also be an inspiring developmental exercise in itself. But how could you quickly find out which combinations are more or less fit? Also, can the complexity of the algorithms be reduced?

These are just some of the interesting questions. This work does not currently provide a complete answer to them, but is experimental one and a research, and any result of it is valuable.

Ukrainian is a language without strict requirements for word order in a sentence (although there are some established preferred options) and has a pleasant sounding. So, it can be a good example and instance. In addition, it is a native language for the author of the programs. Even if you don't want to create and use "phonetic" languages where phonetics is more important than grammar, then you can assess the phonetic potential of the words used in the text to produce specially sounded texts. It can also be valuable and helpful in writing poetry and possible other related fields.

## Sound Representations Durations as the Basis of the Approach

There is the fact as the basis of the approach that the language sounds have different durations, which depends on the many factors e. g. mean of the phoneme producing (the different one for every one of them), other factors that can be more or less controlled but usually the full control is not required and is not achieved. This leads to the fact that chaining of the phonemes and phonetic phenomenae sequentially among which there is also their syllables groupping introduces some rhythmic painting (picture, scheme). A human can (that is also trainable and can be developed) recognize the traits of the picture, compare them one with another, come to some phonetic-rhythmic generalizations and conclusions.

The question of determining the duration of speech sounds is not easy, but the exact result as already mentioned is not required. In this implementation of the approach of phonetic languages, certain statistical characteristics of sounds are used, in particular, possible durations are determined. If we compare the method of determining durations, which is proposed and used in the program of the [r-glpk-phonetic-languages-ukrainian-durations](https://hackage.haskell.org/package/r-glpk-phonetic-languages-ukrainian-durations) package, the analogy will be the packaging of bulky objects. For the observer, the packaging will be an imaginary model of the process of obtaining sound durations. The pldUkr program uses linear programming to find the minimum convex hull (not in a strict mathematical sense), which can 'contain' the sounds of speech. This convex hull has an analogy of packaging, while the sounds of speech have an analogy of objects of variable volume inside the package. The same sound can be used in different situations, in different words with different durations, but the program tries to choose such durations that would 'cover' (similar to the envelope curve 'covers' a family of curves) all these variations for all sounds, with an additional normalization of the duration of the phonetic phenomenon of palatalization (softening) of the consonant, which is least controlled by a human being, and therefore it is expected that this duration is the most resistant to possible random or systematic fluctuations. 

Finally, normalization is not mandatory, it is important that all durations are proportional to each other, i.e. it is not the durations themselves (which are numerically expressed as real positive numbers) that are important, but their mutual ratios (it is allowed to multiply these durations simultaneously by one and the same positive number that does not affect the results of the approach).

## Polyrhythm as a Multi-Ordered Sequence Pattern

Let us have some sequence organized in the following way. Let us implement (generally speaking a conditional one) division of the sequence into compact single-connected subgroups with the same number of elements each in the subgroup, which actually means that we split the sequence into a sequence of subsequences with the same number of elements in each. Consider the internal ordering of each subsequence from the perspective of the placement of the values of its elements and repeatability of the some patterns of the placement of the elements. We assume that the elements can be compared in relation of order, that is, they are the elements of the data type that has an implemented instance of the class Ord.

Considering that the elements of the subsequences may be pairwise different (or in some cases equal), we will compare the positions on which the subgroups of elements that have a higher degree of relatedness (\"closeness\", \"similarity\") in value and order are located. Denote such subgroups by indices that have in the module code mostly a letter designation.

Then each subsequence will consist of the same number of elements of one nature (for example, numbers of the type Double), in each subsequence there will be selected several subgroups of \"similar\" elements in value (and order, if the subsequences are sorted by the value), each of which will have its own index as a symbol (most often in the code -- the characters). Subgroups must have (actually approximately) the same number of elements. Consider the question of positions in the subsequences of the corresponding subgroups in case of they have been belonging to different subsequences.

To assess this, we introduce certain numerical functions that have regular behavior and allow us to determine whether the subsequences actually have elements that belong to the relevant corresponding subgroups in the same places, or on different ones. It can be shown that the situation \"on different ones\" corresponds to the presence of several rhythmimc patterns - for each subgroup will be their own, which do not mutually match, at the same time the ideal situation \"completely in the same places\" corresponds to the case when these rhythms are consistent with each other, as is the case of coherence in quantum physics, in particular spatial and temporal coherence, which is important in particular for understanding of lasers and masers. Polyrhythms consisting of such rhythms, which cohere with each other, form a more noticeable overall rhythm, as well as the presence of coherence in the radiation leads to a more structured latter one.

## Coherent States of Polyrhythmicity as One of the Essential Sources of Rhythmicity

The described pattern of rhythmicity is one of the significant possible options for the formation of rhythmicity in particular in lyrics or music, but not the only one. It should be noted that the described mechanism of rhythm formation, as is noticed in the statistical experiments with texts using this code (the code of the library and its dependent packages on the Hackage site) may not be the only possible option, but in many cases it is crucial and influences the course of the rhythmization process (formation, change or disappearance of the rhythm). It is also known that the presence of the statistical relationship does not mean the existence of deeper connections between phenomenae, in particular -- the causality. \"Correlation does not mean causality.\" A deeper connection implies the presence of other than the statistical ones to confirm it.

## Rap Music Consequences

The code of the library allows in practice to obtain rhythmic patterns that are often close to the lyrics in rap style. Therefore, this can be attributed to one of the direct applications of the library.

## A Child Learns to Read, or Somebody New to the Language

When a child just begins to read words in the language (or, there can be just somebody new to the language) they start with phonemes pronunciation for every meaningful written (and, hence, read) symbol. Afterwards, after some practice, they start to read smoothly. Nevertheless, if the text is actually a poetic piece, e. g. some poem, it is OFTEN (may be, usually, or sometimes, or occasionally, etc.) just evident that the text being read in such a manner has some rhythmicity properties, despite the fact that the phonemes are read and pronounced in a manner of irregular and to some extent irrelevant to the normal speech mode lengths (durations). We can distinguish (often) the poetic text from the non-poetic one just by some arrangement of the elements.

The same situation occurs when a person with an accent (probably, strong, or rather uncommon) reads a poetic text. Or in other situations. The library design works just as in these situations. It assumes predefined durations, but having several reasonable (sensible) ones we can evaluate (approximately, of course) the rhythmicity properties and some other ones, just as the algorithms provided here.

This, to the mind of the author, is a ground for using the library and its functionality in such cases.

## Problem of choosing the best function and related issues

Consider the following question: suppose we have obtained the best version (in our subjective opinion or on the basis of some criteria, it is irrelevant here) of the line in one way or another (here the method does not really matter). Is there a function that makes this particular variant of the string optimal, i. e. for which such a variant of the string gives the maximum of all possible variants generally caused by all possible permutations? Yes, there is. This is easy to prove. The proof is reminiscent of the principle of equalizers.

Let $n \in N$ be the number of syllables in such a line. Arrange the durations of the syllables in ascending order (standard procedure for descriptive statistics). We will find the smallest nonzero difference between adjacent values, divide it by 5. Denote this value by $\delta$. Now consider a number of syllable durations for our best string. Number each syllable from the beginning, counting from 1. Denote by $Y = \{y_{i}, \quad i \in N, \quad i= 1,2, \ldots, n \}$ the set of all values of durations in the order of sequencing in the best line. Denote by $X = \{0 = x_1, x_2, \ldots, x_i, x_{i+ 1}, \ldots, x_{n + 1}, \quad i \in N, \quad i = 1,2, \ldots, n \}$ the set of coordinates of the points of the ends of time conventional intervals, into which our best line divides the time line (the left edge is 0, because the countdown starts with 0). Denote by $M = \{z_i = \dfrac {x_i + x_{i + 1}} {2},\quad i \in N, \quad i = 1,2, \ldots, n - 1 \}$ a set of midpoints of the segments into which the time line is divided by the conditional intervals ends. Denote by $L_1 [a, b]$ the class of Lebesgue-integrable functions on the interval $[a, b]$. Let's mark $I (y_i, z_i, \delta) [z_i - \delta, z_i + \delta]$ class of all bounded functions with $L_1 [z_i - \delta, z_i + \delta]$, the maximum and minimum values of each of which lie on the segment $[y_i -\delta, y_i + \delta]$. We denote each function of class $I$ by $g$.

Consider the class of functions $F$ (a kind of finite analogues of the known delta functions of Dirac), defined as follows:

$$f (x, i) = 
\begin {cases} 
g \in I (y_i, z_i, \delta) [z_i - \delta, z_i + \delta], 
\quad \text {if } y_ {i} \text { is a unique value in the set } Y, \, x
\in [z_i - \delta, z_i + \delta] \\ 
y_ {i}, \quad \text {if } y_ {i} \text {
has equal value with some other number from the set } Y, \, x \in [z_i - 
\delta, z_i + \delta] \\ 
0, \quad \text { otherwise} 
\end {cases}$$

It is easy to see that
$$\sum_{i = 1}^n \int_{-\infty}^{\infty} f(x, i) \, 
\textrm {d} x,$$ where integration is carried out according to Lebesgue, is the desired function (because only the syllables of the best string in their places, taken into account with their indices, give a positive contribution to its value, and for all other variants of the function at least some of the syllables give 0 contribution), and it is not unique due to the fact that in the first line of definition $f(x, i)$ the function can have an arbitrary value from a closed non-empty interval. Therefore, there is at least one class of functions that is described by such a formula, for each of which this particular variant of the string will be optimal.

Let's ask the following question: if we consider not a line, but their combination, for example, a poem. Does a function that makes each line optimal (i. e. which will describe the whole work, each line in it) exist for the whole work (for this whole set of lines)?

In this case, the previous method of constructing the function does not give the desired result, because for two lines it may be that what is best for one of them is not the best for the other. In the case of increasing the number of lines, this general suboptimality only intensifies. However, the existence of such a function for different particular cases is a fundamentally possible situation, however, the probability of its existence should decrease both with increasing number of rows and with the appearance of different features of rows, which increase the differences between them. In general, the search for just one such function may be virtually impractical.

Nevertheless, if we consider the whole work as one line, considering it optimal, then the method just described to construct the corresponding function (class of functions) again gives the result. Yes, the number of syllables in it (in the generalized \"line\"-work) increases, but the procedure itself gives similar results (if we neglect the possible identical durations and repetitions of syllables in a larger text, which lead to the fact that some words can be rearranged and that makes the text only approximately optimal). It is possible to suggest further improvement of this procedure (for example, introduction of the factors which depend on values of the next durations of syllables) that allows to reduce suboptimality of the text.

But still, if you go from one text to another, the resulting function is unlikely to be optimal.

We conclude that for the whole set of expediently organized texts the existence of a universal function is seen as a certain hypothesis (with almost certainly proposes a negative answer).

## Ability to use your own durations of representations of sounds or phonetic phenomena

The programs offer four different sets of phonetic representations by default but it is possible to set your own durations. To do this, specify them as numbers of type 'Word8' (that can be obtained from the 'Double' values from the convex hull envelope method above or from some analogous procedure by the function [zippedDouble2Word8](https://hackage.haskell.org/package/aftovolio-0.8.0.0/docs/src/Aftovolio.General.Datatype3.html#zippedDouble2Word8), see documentation for the [module](https://hackage.haskell.org/package/aftovolio-0.8.0.0/docs/src/Aftovolio.General.Datatype3.html)) in the file in the order defined as follows:

where the specified values in the list refer to the [phonetic representations](https://hackage.haskell.org/package/aftovolio-0.8.0.0/docs/src/Aftovolio.Ukrainian.Syllable.html#showS8) from the module Aftovolio.Ukrainian.Syllable. The last column is 8-bit integers (GHC.Int.Int8), which represent these sounds in the new modules.

If you want to specify several such sets (up to 9 inclusive), you can specify '\*' or several such characters from a new line, and then from the next line there will be a new set of values.

Each set should be in the following order:
\[1,2,3,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24,25,26, 27,28,29,30,31, 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,66,101\]

where the number corresponds to the last column in the above diagram. 101 corresponds to a pause between words (does not affect the search results of the line). Every new value must be written in the file from the new line.

Then when executing the program somewhere among the command line arguments (it does not matter where exactly) specify \<\<+d\>\> \<FilePath to the file with the specified data\>. Programs will read these values and convert them to the appropriate values. 

Along with the custom values, you can use the provided by the library ones, as usual, in the mode of several properties.

## Minimum grammar for possible preservation of meaning and intelligibility

Programs use permutations of words that neglect any (or at least part of) grammatical connections, word order, and so on. This can lead (in addition to the need to think) to situations where grammatically related language constructions are broken, their parts are transferred to other places, forming new connections and changing the meaning of the text.

To reduce this, to eliminate some of these effects, programs use concatenation of words that have a close grammatical connection, so as not to break them in the analysis. This allows you to maintain greater semantic ease and recognizability of the text, as well as a side effect to increase the overall length of the line, which can be analyzed. In the Ukrainian language, grammatically related auxiliary or dependent words precede the independent or main one, so the concatenation of these auxiliary or dependent words to the next one is used. The completeness of the definition of such cases is not exhaustive, but the most frequent cases are considered.

To reduce this, to eliminate some of these effects, programs use concatenation of words that have a close grammatical connection, so as not to break them in the analysis. For the general case, it should be borne in mind that auxiliar or dependent words can go after the independent or the main, so this should be considered separately (and attach such words to the previous, not to the next).

## Advice how to use the programs

In correspondence with the previous information, the rhythmicity of the proposed by the programs variants can be in many cases more evident and perceived better if you read the words (their concatenations) at the lines without significant pauses between them (as one single phonetic flow), not trying to strengthen emphases (probably even without well articulated emphases, smoothly, with liasing). This can be even in a different way, but if the obtained variants do not seem to be rhythmic enough then try just this option and since then compare and come to conclusion whether such sounding is just suitable for your situation.

An interesting additional information about the rhythm and related musical topics is in the video:

1\. Adam Neely. [Solving James Brown's Rhythmic Puzzle](https://youtu.be/DcrYaRJD_e8). Adam Neely. 2021.

# Constraints

An interesting system of constraints was introduced, which allow considering only some subsets of all possible options, which are built taking into account the relative order of words and their place in a line.

When starting the aftovolioUkr program, constraints can be specified as command line arguments. They allow you to reduce the number of calculations, consider only certain options (for example, with a certain defined order of some words, etc.), which allows you to actually expand the capabilities of the program.

There are two options for encoding information about constraints. They use the same basic constraints, but differ in that the extended version allows you to perform algebraic operations on Boolean sets, which allows you to get the complete result, whatever it is, in one input. Instead, the older variant is based on simple linear logic, which can also be sufficient in many cases - all used constraints in the base form must be fulfilled simultaneously, which is equivalent to having a relationship between them through the operator (&&) (boolean \"AND\") and the intersection of the corresponding Boolean sets.

Since words and their combinations in this implementation are assumed to be no more than 7 in a row, all numbers in the basic constraints must be no larger than 7 for the constraint to be \"non-zero\".

It is also worth saying that to get more or less interesting results, it is often advisable to reduce the number of restrictions and use less than necessary.

There are 15 types of basic constraints, and they can be combined in any way, but in compliance with the specifications for each of them. If the specifications are violated, the program will read this constraint as its absence (equal to \"E\").

The basic principle is that the digits indicate the ordinal number of a word or a combination of words written as one word in a string starting from 1 (a natural number), and the letter indicates the type of basic constraint. The rule has an exception -- a \"zero\" constraint (its absence, i.e. the entire set of all possible permutations is indicated), which has only a letter and no digits -- \"E\".

Another necessary condition for the constraint to be non-zero is that no digital symbols within the same encoded constraint cannot be repeated twice. For example, the following constraints are known to be invalid constraints: Q2235 (repeated digits), E2 (numeric characters in a zero constraint where there are none), T2435 (8 is greater than 7). T248 (8 is greater than 7), F1 (one character instead of the required two), A3852 (8 is greater than 7), B5 (one character, but there should be more).

The types of restrictions and their meanings are described in more detail below.

-  Constraint E -- No additional numeric characters - Corresponds to the absence of an additional constraint. Denotes the entire set of all theoretically possible permutations. If used in the extended version (+b \... -b), it can be negated by the symbol \"-\", and then it corresponds together with this symbol to an empty set of permutations, i.e. no actual options are present.

-  Constraint Q -- 4 pairwise unequal digits in the range from 1 to the number of words or their concatenations. The digits are the numbers of 4 words or their concatenations, the mutual order of which will be preserved in the permutations.

  Also, if these words are the same (without taking into account uppercase and lowercase letters), then this is a convenient way to reduce the amount of data to be analysed.

-  Constraint T -- 3 pairwise unequal digits between 1 and the number of words or their concatenations -- The digits are the numbers of 3 words or their concatenations, the mutual order of which will be preserved in the permutations.

  Also, if these words are the same (without taking into account uppercase and lowercase letters), then this is a convenient way to reduce the amount of data that will be analysed.

-  Constraint U -- 5 pairwise unequal digits in the range from 1 to the number of words or their concatenations. The digits are the numbers of 5 words or their concatenations, the mutual order of which will be preserved in the permutations.

  Also, if these words are the same (without taking into account uppercase and lowercase letters), then this is a convenient way to reduce the amount of data to be analysed.

-  Constraint F -- 2 unequal digits between 1 and the number of words or their concatenations -- The digits are the numbers of 2 words or their concatenations, the mutual order of which will be preserved in the permutation.

  Also, if these words are the same (without taking into account uppercase and lowercase letters), then this is a convenient way to reduce the amount of data to be analysed.

-  Constraint A -- 1 digit and several more unequal digits (all unequal to each other) to its right in the range from 1 to the number of words or their concatenations - The first digit is the sequence number of the element, relative to which the placement of all other elements (words or their concatenations); all other digits to the right -- the numbers of the elements that should appear in the resulting permutations TO THE RIGHT of the element with the number equal to the first digit.

-  Constraint B -- 1 digit and several more pairwise unequal digits to its right in the range from 1 to the number of words or their concatenations -- The first digit is the sequence number of the element in relation to which the placement of all other elements (words or their combinations); all other digits to the right -- the numbers of the elements to be placed in the resulting permutations TO THE LEFT of the item with the number equal to the first digit.

-  Constraint P (fixed Point) -- 1 or more several pairwise unequal digits in the range from 1 to number of words or their concatenations -- each of them means the ordinal number of the word that will remain in its place during permutations place.

-  Constraint V -- (as half of W) -- 2 unequal digits in the range from 1 to the number of words or their concatenations, each of which means the ordinal numbers of two words, the distance between which and the mutual order of which will be preserved in all permutations. Most often it can be used for two neighbouring words and means then, that they will be present as such a pair in all the variants being analysed. It is also important if you don't want to concatenate such words so as not to change the syllables of their common (\"tangent\") boundaries at the same time.

-  Constraint W -- (from the word tWo -- 2) -- 2 unequal digits in the range from 1 to the number of words or their concatenations, each of which means the ordinal numbers of the two words, the distance between which will be preserved in all permutations. Most often it can be used for two neighbouring words and means then, that they will be present in all variants together, but their relative order can be also reversed.

-  Constraint H -- (from the word tHree -- 3) -- 3 unequal digits in pairs in the range from 1 to the number of words or their concatenations, each of which means the ordinal numbers of the three words, the distances between which and the mutual order of which will be preserved in all permutations. A certain subset of the constraint R. Most often used for three words in a row.

-  Constraint R -- (from the word thRee -- 3) -- similar to H, but only the distances between the first two words are preserved, and the same for the distances between the second and third words. Their order can be changed to reverse. It's a rather complicated combination, but it can be important in terms of grammar, as well as the string to be analysed.

-  Constraint M -- (from the word Mixed) -- mixed H-R constraint -- similar to H, but between the first two words are preserved both the distance and their relative position, and only the distance is preserved between the second and third ones. It is also a rather complex combination, a certain subset of the constraint R.

-  Constraint N --- (fixed poiNts) --- an even non-zero number of digits, where each digit in an odd position (first digit, third, fifth, etc.) means the ordinal number of the word or combination in the string that must be fixed in a new position, and the next digit is the new ordinal number of this word or combination; thus, if this pair of digits is the same, it means that this word should remain in its place, otherwise it will be fixed in a new position. Generalised constraint P, where the user can change the position of words and their combinations in the string and fix them.

-  Constraint D --- (DistanceD worDs) --- three digits, the first two of which must be unequal and indicate the sequence numbers of two words or their combinations, which must preserve the mutual order, and the third digit is the new distance between these words (1 means that the words are consecutive, 2 means that there is one other word between the words, etc.), it must be in the range from 1 to the number of words and their combinations minus 1. A generalisation of constraint V, where the user can specify a new distance between words.

-  Constraint I --- (dIstanced words) --- is similar to D, with the difference that the words can be in reverse order. Therefore, in the symbolic notation, Iabc == Ibac. Also, Dabc + Dbac == Iabc. A generalisation of the W constraint, where the user can specify their own distance between words.

-  Constraint G --- (Groupped together) --- two unequal digits, each of them indicates the sequence number of the first and the last elements of the group of words or their concatenations that should be groupped together as a one whole (with possible inner permutations inside the group) during all the permutations. Is useful mostly for some syntactic constructions such as a sentence or a phrase. The inner counting are not implemented yet and probably will be implemented in the future releases. Now the group is just used as a non-interrupted groupping of the words to be preserved as a one whole.

## Simple linear \"AND\" logic

If only one of the basic constraints is used, or it is known for sure that it is necessary that all basic constraints in their set are fulfilled simultaneously, then you can use this option. It is logically the same as the one implemented previously.

To do this, you use +a \... -a \"brackets\" between which there are constraints in their record, and all of them are separated from each other by a space. For example:

-  +a P23 A456 -a -- means that Ρ23 and Α456 are satisfied simultaneously, i.e. that the second and third words must remain in their places (constraint P), and that the fifth and sixth words must come after the fourth (constraint A).

-  +a B23 F45 E T356 -a means that B23, F45, E and T356 are satisfied simultaneously, i.e. the third word must come after the second, the fifth after the fourth, a \"zero constraint\" (meaning that it can be omitted), and the relative order of the third, fifth and sixth words must be preserved.

In fact, the space character between the constraints in this variant means a logical \"AND\".

If you need to set more complex constraints, or in general, in view of the possible faster work with the program, you should use the general algebraic version of setting constraints.

## Algebraic (universal) variant of constraints

I would like to point out that in order to use this functionality efficiently, you need to know the basics of logic and/or set theory, but these materials are widely available in different languages, including Ukrainian and English, at different levels of complexity. As for mathematics, you should at least understand what logical AND (conjunction), logical OR (disjunction), logical negation (NOT), universal set, and complementary set are. If you know how to program in at least one language, you are also familiar with these concepts and have used them.

To use the algebraic version of constraints, you use +b \... -b \"brackets\" between which the constraints in their record are placed, and you can additionally use brackets and the \"-\" sign, which means a logical negation (logical \"NOT\") of the next after minus expression in brackets or the basic constraint. Parentheses, as is often the case, indicate the order of calculation, as in algebra. After the minus, there must be either an open parenthesis and an expression or an underlying constraint.

If the constraints are written consecutively without spaces, the program understands this as a \"multiplication\" of the constraints, i.e. a logical \"AND\", and if there is a space between them, it is an \"addition\" (logical \"OR\").

There can be no more than 1 \"-\" in a row, and there can be only one space between groups of constraints. Parentheses must be opened and closed according to the general rules of algebra (for example, the number of open and closed parentheses must be equal to each other). If these rules are violated, the program will display a message that it is impossible to output options that meet these constraints and ask you to specify other data and constraints. A bracketed group must contain at least one basic constraint. The priority of operations is the same as for logical operators in Haskell and in Boolean logic in general.

Note: if you use Linux shells (e. g. bash) or Windows PowerShell then for the usage of parentheses you need to use quotation marks for the whole generalized algebraic constraint. Please, refer to the further documentation for your shell or PowerShell.

Examples of correct constraints:

-  +b '(A23-H345 P45-(A24 B56))' -b is a symbolic representation of a Boolean set

    (Α23 && not H345 || P45 && not (A24 || B56)),

  where 'not' means a completion to the set in the universal set of permutations, i.e. here it means all those theoretically possible permutations, except for those denoted by the expression after \"-\". Mathematically, this can be written as: $$(A23 \land \neg H345 \lor P45 \land \neg (A24 \lor B56))$$

-  +b A345B62 P4-Q3612 -b is a symbolic representation of a Boolean set

    A345 && B62 || P4 && not Q3612,

  which is mathematically equivalent to: $$A345 \land B62 \lor P4 \land \neg Q3612$$

-  +b '(E)-P4' -b is a symbolic representation of a Boolean set

    (E) && not P4,

  which is mathematically equivalent: $$U \land \neg P4 = \neg P4,$$ where $U$ is the universal set of all theoretically possible permutations.

Examples of mis-specification of constraints -- if in each of the previous examples above remove one character except for numbers and spaces, or write two consecutive minuses or spaces anywhere. Or open and close brackets without letters inside.


# Why some lines are easy to pronounce and others are not, or Prosodic unpredictability as a characteristic of text

## Unpredictability in reading and speaking

### Main observations and the idea

Let’s take a look at how the software works and why the strings it offers in different parts of its output can be read and pronounced so differently, even though they consist of the same words (almost the same sounds, if we do not take into account possible phenomenae of assimilation and similarity).

The author of the software noticed that when working with the program for different options of a line, unexpected pauses may appear in the program output, during which a certain change in the rhythm of speech occurs (the previously mentioned incongruities). Such pauses for an experienced reader (speaker) do not (may not) occur within words, but only between words and their combinations.

The phenomenae of perception and apperception are known to be combined.

During perception, a person partially "guesses" the part of the line that he or she is going to read and prepares his or her speech apparatus for this.

It can be reasonably assumed that the appearance or absence of such pauses is due to an unpredictable change in the line, which does not correspond to the pattern to which the person has been attuned by their combination of aperception and perception.

One of the factors that affect apperception is specifically the presence of a previously detected rhythmic pattern, and therefore, the appearance of these pauses is more likely to occur when this pattern changes. Nevertheless, it has also been observed that a person can learn to read (pronounce) a line as a single phrase without the presence of these pauses, if he or she makes a certain effort and also as a result of training (repeated readings of one of the lines with memorisation of the structure). Also, the number of such pauses is significantly reduced if you read different options of the same line in a row (one can think that a person is preparing for such changes, which they already partially know). Let us dwell a little more on "guessing" (prediction).

If, as a result of a combination of apperception and perception, a person "guesses" what the next speech structure will be, the "fluidity" (smoothness) of the line, then there is no such pause, the text is read (pronounced) further according to the combination of perception and apperception by the already prepared speech apparatus, which minimises delays.

If, on the other hand, perception creates an instantaneous "false prediction" of what the further speech construction, the "smoothness" of the line will be, then for expressive speech, the speech apparatus has to be readjusted on the fly, first detecting this discrepancy between the aperceptual predicted pattern and the actual construction of the text, and only then readjusting to this change and forming a new perception, a new forecast for the future. These latter processes take more time, and therefore an additional, prolonged pause and the emergence of a feeling of a "break" in the line, intonation, difficulty of pronunciation, etc.

It can also be assumed that for an inexperienced reader or speaker who reads (pronounces) line by syllables, slowly, such additional delays should be almost non-existent, since they are much less likely to use apperception, but mostly perception.

### Analysis of the prediction pattern

What is the pattern of prediction?

The software offers variants in which there is a certain repeatability (or its somewhat unpredictable change) of the relative relative placement of syllables (time elements in the more general case) wdifferent durations, and allows also, in certain cases, to say what these patterns will be approximately. Perception, when repeated several times (for several metrical feet) or at least once in a line of a certain sequence of durations, causes an expectation, a forecast for the continuation of such repetition as something more likely, i.e. recognises a certain pattern of rhythmicity, and causes a certain preparation for it. 

## The case of a two-syllable metrical foot

Let’s take the case of a two-syllable foot as an example. If you run the program by calling

aftovolioUkr +r 21 \<Ukrainian text\>

then this is exactly the case when the program will analyse two-syllable feet.

It often happens that the value of the property in this case for several variants will be 0.

What does this mean? If you look at the code, you can see that this corresponds to the case when each subsequent metric foot, starting with the first in the string, offers a modified version of the foot to a mirror-opposite one if the syllable durations in it are not the same (which is the case in the vast majority of cases). This means a consistent, constant alternation of the iamb and the chorus according to one of two options.

In the first case, the first foot is a chorus, and in the second -- an iambic. Then in the first case, such a line will have the structure (except for the first syllable, which is "extra") -- pyrrhic -- spondee - pyrrhic - spondee \.\.\., and in the second case (except for the "extra" first syllable) -- spondee -- pyrrhic -- spondee -- pyrrhic \.\.\.

This leads in many cases to a more undulating intonation, with short groups of longer and shorter syllables alternating (except for the first syllable). Moreover, the ending of such a line can contain either a full spondee or pyrrhic (if the number of syllables in the line is odd) or only half of it (if the number of syllables in the line is even).

The opposite options in the output of the program will be those for which the property value will be the maximum possible for these words. This, in turn, means that if this number is equal to the theoretical maximum (the quotient of the number of syllables in a line and 2), then the line is a perfectly constructed iambic or chorus (according to its metrical properties), except, perhaps, for its immediate ending (the last syllable if the number of syllables is odd). This often leads to the fact that such a line is often read in accordance with a distinctive iambic or chorus pattern - often quite smoothly, without the above-mentioned pauses.

In practice, the first option (with a value of 0) is very common, almost always is present, but the latter is rare. More commonly, it is not possible to create a perfect iambic or chorus for the given words (but if you replace them with other, more appropriate ones, you can get a perfect line in many cases due to the synonymous richness of the language).

However, if the maximum value of a property differs from the maximum theoretically possible by an even number (2, 4, 6, etc.), then the feet at the beginning and end of the line are the same (except possibly the last syllable if the number of syllables is odd), and inside the line they alternate as many times as this number differs from the maximum possible number. If the maximum number differs from the maximum theoretically possible by an odd number (1, 3, 5, etc.), then at the beginning and end of the line - different feet (except, perhaps, the last syllable if the number of syllables is odd

In these "close to ideal" cases, pauses are often possible, but also possible pauses are often present, but they can also be absent, depending on where the changes in the feet occur (between words or within words), and whether it is correctly predicted.

If we consider the intermediate values of the property, we get less predictable line options, which can still be reasoned about in a similar way to the second case (and the closer the value of the property is to 0, the more the line becomes similar to the first case with a more undulating structure).

# Stylistic peculiarities in language and versification of the Taras Shevchenko’s poem ”Садок вишневий коло хати” using AFTOVolio

Let's consider Taras Hryhorovych Shevchenko's poem \"Cherry orchard around the house\.\.\.\" in relation to the author's style, and more precisely, the peculiarities of language and versification. To do this, we will use the programs of the aftovolio package from the Hackage website.

Next, it is shown how you can analyze the text, knowing how to work with the programs of the package, and what conclusions this leads to.

## Analysis

Let's try to use the property y0 with a selective sum to analyze the original version of the poem \"Садок вишневий коло хати\" by Taras Shevchenko.

Let's pay attention to the second line - \"Хрущі над вишнями гудуть.\" In it, the words \"хрущі\" and \"гудуть\" with \"у\" are placed as far as possible from each other. This suggests that this line satisfies the property y0.y. We check:

    aftovolioUkr +ul y0.у Хрущі над вишнями гудуть
    7 Хрущі гудуть надвишнями  3    7 надвишнями Хрущі гудуть  4
    6 надвишнями гудуть Хрущі  2    18 гудуть надвишнями Хрущі  5

Indeed, 19 out of a maximum possible 19! But it can be noticed that the vowels in the words are selected so that they do not repeat closely, so the maximum can be expected not only for y0.y, but also for the more general property y0.vw:

    aftovolioUkr +ul y0.vw Хрущі над вишнями гудуть
    7 Хрущі гудуть надвишнями  3    7 надвишнями Хрущі гудуть  4
    6 надвишнями гудуть Хрущі  2    18 гудуть надвишнями Хрущі  5
    6 гудуть Хрущі надвишнями  1    19 Хрущі надвишнями гудуть  6

Again the maximum 19 out of 19 possible! Okay, that's it for this line. And for the whole poem? If we check every line with 
  
    aftovolioUkr +ul y0.vw
  
command, then we see many maximums. 

We check: indeed, many maximums! But not all\.\.\. Let's try to add something or take something away to increase the number of maxima, then this new property will be the most appropriate. Let's see where there are no maxima and why. For example, lines in a row --- \"А матері вечерять ждуть. // Сем'я вечеря коло хати // \" --- in them we notice that the sounds \"е\" are close to each other. Then if you take away the \"е\", maybe it will be closer to the maximum? We check:
  
    aftovolioUkr +ul y0.а.о.у.и.і
  
We pay attention, indeed, for these lines there was a shift towards maximums, the last line \"Та соловейко не затих\" also \"risen\" to the maximum (admittedly, from two possible options). Instead, the line \"Дочка вечерять подає\" \"dropped\" from the maximum to the 3 interval. It can be assumed that the \"status\" of \"е\" in the poem is \"unclear\": with or without it.

As you can see, it is not possible to achieve maximums everywhere under various options. This is especially true of the three lines in a row at the end: \"Поклала мати коло хати // Маленьких діточок своїх // Сама заснула коло їх\" And also \"Співають ідучи дівчата // А матері вечерять ждуть.\" In the second case, can see that \"у\" alternates with \"i\", that is, we have \"ю(у) - i - y\", as well as in the next \"e - i - e - e\". It can be assumed that there is 'art sound painting' here - vowels alternate to reproduce the motifs of the melody and rhythm of folk songs. Similarly, it can be assumed that before the end of the poem, the vowels begin to \"group\" closer, that is, a \"coherent, dynamic\" effect is created, that is, amplification, and it can be thought that it describes a certain change in the situation. Let's see, it can really be called the highest point (climax) of the poem, and therefore its amplification by grouping of vowels creates the effect of the top of the picture. Accordingly, it is necessary to read (recite) these lines (this is justified, see the title) with a certain acceleration, not measured, but more cohesively. At the same time, es where we have a maximum for vowels (or vowels with deaf consonants) should be read more measuredly, so that it is possible to see the richness of the painting, its calmness and stability. The penultimate line has a pause in the middle---like a pause after a highest point (climax). And then the denouement.

It must be said that, having 3-4, sometimes 2 words in a line, in order not to create the impression of \"short lines\", a certain \"ungrouping\" of sounds should be applied, so that it can be perceived as a leisurely narration (as in this case).

# Peculiarities of the stressed and unstressed syllables in Ukrainian and other languages

In linguistic typology, there is a prosodic classification of languages according to the rhythm of oral speech. In this sense, speech rhythm is interpreted as a periodic isochrony (temporal and quantitative identity) of comparable phonetic units.

It is believed that all the world's languages can be divided into three groups by the type of rhythm (isochrony of comparable units), depending on which unit manifests regular (permanent) isochrony (Ladefoged 1975 (Ladefoged, Peter. "A Course in Phonetics. Harcourt Brace Jovanovich." New York (1975)), Ishchenko 2015, see link below):

- syllable-timed languages: languages of syllable rhythm, that is, in which syllables - stressed and unstressed - have a relatively uniform duration. This group includes primarily the Romance languages (Spanish, French, Italian, etc;)

- stressed-timed languages: languages of accent rhythm in which isochrony is manifested at the level of relatively uniform duration of intervals between adjacent stressed syllables (interaccent intervals). These include Germanic, Arabic, and Slavic languages;

- mora-timed languages: languages with a mora rhythm, i.e. in which isochrony is inherent in the mora. These are primarily Japanese, as well as some languages of Austronesian, Nigerian-Congolese families. It is also believed that that Proto-Indo-European, ancient Greek, and Vedic Sanskrit were also mora-timed languages.

For the Ukrainian language, Oleksandr Ishchenko's study ([Іщенко, О.С. (2015). Українська мова в просодичній типології мов світу. Dialog der Sprachen, Dialog der Kulturen. Die Ukraine aus globaler Sicht: V. Internationale virtuelle Konferenz der Ukrainistik (S. 76-85). München: Verlag Otto Sagner](https://phonetica.wordpress.com/wp-content/uploads/2015/11/ukrajinska-mova-v-prosodychniy-typologiji-mov-svitu.pdf)) found that the average duration of stressed syllables in slow speech was 240 ms ± 31%, and in fast speech - 145 ms ± 35%. The average duration of unstressed syllables in slow speech is 175 ms ±40%, in fast speech - 110 ms ± 34%. The difference between the duration of stressed and unstressed syllables in slow speech is 39%, in fast speech - 28%.

Therefore, for the Ukrainian language it is appropriate to use =13 or similar values, e. g. =133 etc. for the stressed syllables to specify that they are stressed while using the aftovolioUkr program.

See, for example:

    aftovolioUkr -e +r 21 садок=133 вишне=133вий ко=133лоха=133ти хрущі=133 над ви=133шнями гудуть=133
    ...
    8 садок вишневий колохати надвишнями гудуть хрущі  711
    ...
 
as the element with the maximum value (the "-e" command line argument suppresses output of "={digits}" for better readability). The second line is changed because of the last syllable in the first line that is redundant as for the iambic meter.

You can easily paste the =133 group into thethe last one groups since the version 0.8.0.0 text in many editors or on the command line directly.

## Conclusions

It can be concluded that Taras Shevchenko wrote the lines of the poem in such a way as to place vowels more widely, as well as voiceless consonants (where there are not enough vowels, for melodiousness), but there, perhaps, there was more dynamism --- there, on the contrary, he chose options with their closer grouping . Therefore, these lines should be read more dynamically.

# Usage of the executable

SYNOPSIS:

- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [+c \<HashCorrections encoded\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+k \<number - hash step\>] [+r \<groupping info\>] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+nm \<numbers of the lines in the sorted AftovolioUkr list to be displayed\> -nm] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>]] Ukrainian textual line

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [+c \<HashCorrections encoded\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+k \<number - hash step\>] [-t \<number of the test or its absence if 1 is here\> [-C +RTS -N -RTS]] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+x \<maximum number of words taken\>]] Ukrainian textual line

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [+ul \<diversity property encoding string\>] [+l] [-p] [+w \<splitting parameter\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>]] Ukrainian textual line

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+q \<power of 10 for multiplier in [2..6]\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+nm \<numbers of the lines in the sorted AftovolioUkr list to be displayed\> -nm] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>] [+l2 \<a Ukrainian text line to compare similarity with\> -l2]] [+di] Ukrainian textual line

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+q \<power of 10 for multiplier in [2..6]\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+nm \<numbers of the lines in the sorted AftovolioUkr list to be displayed\> -nm] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>] [+ln \<a sequence of positive Word8 values not greater than 255 e. g. 24 157 45 68 45 56 59 to compare similarity with\> -ln]] [+di] Ukrainian textual line

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+q \<power of 10 for multiplier in [2..6]\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+nm \<numbers of the lines in the sorted AftovolioUkr list to be displayed\> -nm] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>] [+di] [+m \<FilePath\> \<num1\> +m2 \<num2\>]]

OR:
- aftovolioUkr [[+a \<AFTOVolio constraints\> -a] [+b \<extended algebraic AFTOVolio constraints\> -b] [+P \<non-negative Int\>] [-e] [+l] [+d \<FilePath to file with durations\>] [+s \<syllable durations function number\>] [-p] [+w \<splitting parameter\>] [+q \<power of 10 for multiplier in [2..6]\>] [+f \<FilePath to the file to be appended the resulting String\> \<control parameter for output parts\>] [+x \<maximum number of words taken\>] [+nm \<numbers of the lines in the sorted AftovolioUkr list to be displayed\> -nm] [+dc \<whether to print \<br\> tag at the end of each line for two-column output\> \<FilePath to the file where the two-column output will be written in addition to stdout\>] [+di] [+m3 \<FilePath\> \<num1\> \<num2\>]]

OR:
- aftovolioUkr [-cm \<FilePath to write the resulting combined output to\> \<FilePaths of the files to be compared and chosen the resulting options line-by-line\>]

### Executable commandline parameters and their meaning

- +l   — if specified then the output for one property (no tests) contains empty lines between the groups of the line option with the same value of property.

- +w   — if specified with the next Int8 number then the splitting of the output for non-testing options is used. Is used when no "-t" argument is given. The output is split into two columns to improve overall experience. The parameter after the "+w" is divided by 10 (-10 for negative numbers) to obtain the quotient and remainder (Int8 numbers). The quotient specifies the number of spaces or tabular characters to be used between columns (if the parameter is positive then the spaces are used, otherwise tabular characters). The remainder specifies the option of displaying. If the absolute value of the remainder (the last digit of the parameter) is 1 then the output in the second column is reversed; if it is in the range [2..5] then the output is groupped by the estimation values: if it is 2 then the first column is reversed; if it is 3 then the second column is reversed; if it is 4 then like 2 but additionally the empty line is added between the groups; if it is 5 then like for 3 and additionally the empty line is added between the groups. Otherwise, the second column is reversed. The rules are rather complex, but you can give a try to any number (Int8, [129..128] in the fullscreen terminal). The default value is 54 that corresponds to some reasonable layout.

- +s   — the next is the digit from 1 to 4 included. The default one is 2. Influences the result in the case of +d parameter is not given.

- +d   — if present, then afterwards should be a FilePath to the file with new durations of the Ukrainian AFTOVolio representations. See the information above in the section about durations.

- -p   — if present the minimal grammar transformations (appending and prepending the dependent parts) are not applied. Can be useful also if the text is analyzed as a Ukrainian transcription of text in some other language.

- +f   — if present with two arguments specifies the file to which the output information should be appended and the mode of appending (which parts to write). The default value if the secodnd parameter is 0 or not element of [1,2,3,4,10,11,12,13,14,15,16,17,18,19] is just the resulting String option. If the second parameter is 1 then the sequential number and the text are written; if it is 2 then the estimation value and the string are written; if it is 3 then the full information is written i. e. number, string and estimation; if it is 4 then the number and estimation (no string).
The second argument greater or equal to 10 takes effect only if the meter consists of two syllables (in case of "+r 21" command line options given). If it is 10 in such a case then before appending the line option itself to the given file there is hashes information for this line option displayed. If it is 11 — the same hashes information is displayed before processing as for 1. If it is 12 — the same hashes information is displayed before processing as for 2 and so on for 13, 14. If it is 15 up to 19 — it is analogically to the the 10–14 but the hashes information is not only printed on the screen, but appended to the file specified. These values are intended to test the interesting hypothesis about where the pauses can occur. For more information on the hypothesis, see above in the section about prosodic unpredictability.

- +nm \.\.\. -nm   — if specified in the modes without tests and file single line output changes the output so that just the lines with the specified Int numbers are displayed in the order of the specified numbers. Please, delimit the numbers with spaces. To specify some range, use just dash as here: \'34-250\' meaning that lines with numbers between 34 and 250 inclusively will be displayed. Music mode additional information is displayed as well here.

- +dc   — if specified with two further arguments then the first one can be 1 or something else. If it is 1 then additionally to every line as usual there is printed also \<br\> html tag at the end of the line for the two-columns output. Otherwise, nothing is added to each line. The second argument further is a FilePath to the writable existing file or to the new file that will be located in the writable by the user directory. The two-column output will be additionally written to this file if it is possible, otherwise the program will end with an exception.

- +a \.\.\. -a    — if present contains a group of constraints for AFTOVolio. For more information, see above.

- +b \.\.\. -b    — if present takes precedence over those ones in the +a \.\.\. -a group (the latter ones have no effect). A group of constraints for AFTOVolio using some boolean-based algebra. If you use parentheses there, please, use quotation of the whole expression between the +b and -b (otherwise there will be issues with the shell or command line interpreter related to parentheses). For example, on Linux bash or Windows PowerShell: +b 'P45(A345 B32)' -b. If you use another command line environment or interpreter, please, refer to the documentation for your case about the quotation and quotes. For more information, see above.

- +P  — if specified with the following non-negative Int from 0 to 3 including then specifies the general type of permutations of the words and their concatenations. +P 0 corresponds the full set of all possible permutations (the default behaviour, also in case of no specification at all), +P 1 corresponds to the set of permutations, where just one word can change its position (the elementary, the least possible permutation), +P 2 corresponds to the set of permutations, where two words can be swapped one with another, +P 3 corresponds to the set of permutations, where no more than two words can change their positions, including the cases of no changes at all and just one word changes its position. All, except +P 0 provide less permutations in general and are quicker to be computed and displayed. This one is more useful in case of extended sets of words, e. g. when there are 8 or 9 words in the line.

- +l2 \.\.\. -l2   — if present and has inside Ukrainian text then the line options are compared with it using the idea of lists similarity. The greater values correspond to the less similar and more different lines. Has no effect with +dc group of command line arguments. Has precedence over +t, +r, +k, +c etc. groups of command line options so that these latter ones have no effect when +l2 \.\.\. -l2 is present.

- +ln \.\.\. -ln   — if present and has inside a sequence of positive Word8 values not greater than 255 then the line options are compared with it using the idea of lists similarity. The greater values correspond to the less similar and more different lines. Has no effect with +dc group of command line arguments. Has precedence over +t, +r, +k, +c etc. groups of command line options so that these latter ones have no effect when +ln \.\.\. -ln is present.

- +di   — if present implies the "differentiation" mode of computation for the comparing options with the line in +l2\.\.\.-l2 or +ln\.\.\.-ln groups of command line arguments. Is useful mostly in case of the line to compare with has approximately the same number of syllables as the option lines.

- -e   — if present suppresses the printing of "={digits}" and \"\_{digits}\" (the last one groups since the version 0.8.0.0) in the line option to the screen. Is used for better readability, often with +di.

- +q   — if present with +l2 \.\.\. -l2 group of arguments then the next argument is a power of 10 which the distance between line option and the predefined line is quoted by. The default one is 0 (that means no change). You can specify not less than 0 and not greater than 4. Otherwise, these limit numbers are used instead. The greater value here leads to more groupped options output.

- +ul   — afterwards there is a string that encodes which sounds are used for diversity property evaluation. If used, then +r group has no meaning and is not used. Unlike in the link, the argument "1" means computing the property for all the sound representations included (for all of the present representations, so the value is maximal between all other strings instead of "1"). For more information, [see](https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types)

- +r   — afterwards are several unique digits not greater than 8 in the descending order — the first one is the length of the group of syllables to be considered as a period, the rest — positions of the maximums and minimums. Example: "543" means that the line is split into groups of 5 syllables starting from the beginning, then the positions of the most maximum (4 = 5 - 1) and the next (smaller) maximum (3 = 4 - 1). If there are no duplicated values then the lowest possible value here is 0, that corresponds to the lowest minimum. If there are duplicates then the lowest value here is the number of the groups of duplicates, e. g. in the sequence 1,6,3,3,4,4,5 that is one group there are two groups of duplicates — with 3 and 4 — and, therefore, the corresponding data after +r should be 7\.\.\.2. The values less than the lowest minimum are neglected.

- +c   — see explanation at [the link](https://hackage.haskell.org/package/rhythmic-sequences-0.8.0.0/docs/src/Rhythmicity.MarkerSeqs.html#HashCorrections). Some preliminary tests show that these corrections influence the result but not drastically, they can lead to changes in groupping and order, but mostly leave the structure similar. This shows that the algorithms used are more stable for such changes.

- -t   — and afterwards the number in the range [0..179]  (with some exceptions) showing the test for 'smoothness' (to be more accurate - absence or presence of some irregularities that influences the prosody) to be run - you can see a list of possible values for the parameter here at the link:
[link1](https://hackage.haskell.org/package/aftovolio-0.8.0.0/docs/src/Aftovolio.Tests.html#sel). For ideas of actual usage of the tests, see the documentation above.

- -C   — If specified with +RTS -N -RTS rtsoptions for the multicore computers with -t option then it can speed up the full computation of the all tests using more resources. Uses asynchcronous concurrent threads in computation. This groups (-C and rtsoptions) can be specified in any position as command line groups of arguments. The output is in this case not sorted in the order of 2,3,4,5,6,7, but can vary depending on your CPU configurations and states and / or some other factors.

- +k   — and then the number greater than 2 (better, greater than 12, the default value if not specified is 20). The greater value leads to greater numbers. The number less than some infimum here leads to wiping of some information from the result and, therefore, probably is not the desired behaviour. For most cases the default value is just enough sensible, but you can give it a try for other values.

- +x   — If specified with the further natural number then means that instead of maximum 7 words or their concatenations that can be analysed together as a line there will be taken the specified number here or 9 if the number is greater than 8. More words leads to more computations, therefore, please, use with this caution in mind. It can be useful mostly for the cases with the additional constraints specified (+a \.\.\. -a or +b \.\.\. -b groups, see above).

- -cm   — If present, the program works in a special comparative mode reading information from the several files line-by-line and prompting to choose the resulting option from the files given. If some files do not have such lines, then the resulting option for the file is empty. You choose the resulting option by typing its number on the terminal. The total number of sources is limited to no more than 14.

- +m   — If present followed with two arguments — the name of the file to be used for reading data for multiline processment and the second one — the number of line to be processed. The numeration of the lines in the file for the aftovolioUkr program here starts from 1 (so, it is: 1, 2, 3, 4,.. etc). The program then uses instead of text the specified line from the file specified here and prints information about its previous and next lines (if available).

- +m2   — If present, it means that the line with the corresponding number specified here will be taken from the same file specified as +m \<file\>. For example, the entry +m \<file\> 1 +m2 4 means that 1 line will be taken from the file \<file\> for the similarity analysis and it will be compared not with contents of +l2 \.\.\. -l2, but with the 4th line from the same file.

The abbreviation for +m \<file\> \<num1\> +m2 \<num2\> is +m3 \<file\> \<num1\> \<num2\>, which has the same meaning but a slightly shorter entry.

If \<num1\> == \<num2>, then there is at least one of all the options with a property value of 0.

# "Music" mode of operation and extended possibilities

You can also use the "music" mode, which allows you to write better lyrics and music. To do this, you can add a record of the form \_{set of digits} or ={set of digits} to a word or between syllables after the needed to be referred to, and this set of digits will be converted to a non-negative Double number by the program and then used to modulate the duration of the previous syllable or of the additional one(s) added here. The first digit in the record after '=' or '\_' is a whole number and the rest is a fraction (mantissa).

If you have added \_{set of digits} then this number will be multiplied by the duration of the syllable to which this insertion refers, and the resulting number will be rounded so that is can be no more than 255 (the maximum possible value of Word8 datatype that is used internally to calculate the statistics). Afterwards, this value is placed among the others in the place where this entry is inserted.

If you have added ={a set of digits} then this number will also be multiplied by the duration of the syllable to which this insertion refers, and the resulting number will be rounded and then will be placed instead of the duration that it was multiplied by (again, the maximum resulting value here can be 255, so that the greater values are rounded to this limit). This allows to make syllables longer or shorter to follow language or creative intentions and rules (e. g. in French there have been noticed that the duration of the last syllable in the word, especially in the sentence or phrase is prolonged to some extent, so you can use '=12' or '=13' or '=14' here). Not as for the \_{set of digits} group, you can place it only after the syllable and only once to change the duration of the syllable. If you use this option, the following \_{set of digits} will be referred to the new changed duration here, so that "так"=2\_05\_1 will mean that you will use instead of the usual duration of the syllable "так" the twice as much duration, and then half of this twice as much (therefore, equal to duration of the syllable without altering)and then again the twice as much duration etc. The insertion of =1, =10, =100, =1000 etc will not change anything at all, except for the time of computations (you probably do not need this).

These two additional possibilities allows you to change rhythmic structures, and interpret this insert as a musical pause or, conversely, a musical phrase inserted to change the rhythm. What syllable does this insertion refer to? It refers to the previous syllable in the line. There can be several such inserts, their number is not limited by the program itself (in the program code), but a significant number of inserts can significantly increase the duration of the calculation.

When the results of the program are displayed on the screen, these inserts will also be displayed in the appropriate places.

If you specify \_1, \_10, \_100, \_1000, etc. as such an insertion, the program will assume that this insertion duration is equal to the duration of the syllable to which it refers. If the set of digits is preceded by a 0, the insertion has a shorter duration, if the 1 in the first 4 examples above is followed by digits other than 0, or if another digit is used instead of 1 or 0, the insertion has a longer duration. For example, \_05 means exactly half the duration of the syllable, and \_2 means double the duration. In any case the resulting values cannot be greater than 255, the greater ones are rounded to 255.

For the "music" mode the number of syllables printed for the line does include the inserts.

