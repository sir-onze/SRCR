%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Parte 1
% - Configurações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- op(900,xfy,'::').

% - Definicoes iniciais de entidades presentes na base de conhecimento:

%utente: IdUt, Nome, Idade, Cidade -> {V,F}

%serviço: IdServ, Descrição, Instituição, Cidade -> {V,F}

%consulta: Data, IdUt, IdServ, Custo -> {V,F}

%--------------------------------- Base de conhecimento - - - - - - - - - -  -  -  -  -   -
% - Utentes

utente(12345,joaocarlos,20,braga). 
utente(67810,martagalo,20,barcelos).
utente(11123,carlamuamba,20,luanda).
utente(14156,pedroavec,20,paris).
utente(17189,joaodostrespes,20,braga).
utente(20212,ricardoultra,20,guimaraes).
utente(23245,joanatripeira,20,porto).
utente(26278,joaodasfrigideiras,20,braga).
utente(28293,margaridaleao,20,braga).
utente(31323,fredericasantos,20,beja).
utente(33345,paulotabuada,20,sintra).
utente(51523,adrianodovitoria20,aveiro).

% - Serviços

