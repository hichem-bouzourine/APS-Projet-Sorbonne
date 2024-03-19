#!/bin/bash

# Fonction pour imprimer en couleur
print_color() {
    echo -e "\e[$1m$2\e[0m"
}

# Itérer sur tous les fichiers .aps dans le répertoire Samples et ses sous-répertoires
find Samples -type f -name '*.aps' | while read -r file; do
    # Exécuter prologTerm pour le fichier en cours
    result=$(./prologTerm "$file" 2>&1)

    # Vérifier si le résultat contient le message d'erreur spécifié
    if [[ $result == *"Fatal error: exception Stdlib.Parsing.Parse_error"* ]]; then
        # Imprimer le nom de fichier en blanc
        print_color "0;37" "$file"
        # Imprimer le résultat en rouge
        print_color "0;31" "$result"
    else
        # Imprimer le nom de fichier en blanc
        print_color "0;37" "$file"
        # Imprimer le résultat en vert
        print_color "0;32" "$result"
    fi
done