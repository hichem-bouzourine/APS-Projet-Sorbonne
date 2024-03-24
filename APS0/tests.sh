# Fonction pour imprimer en couleur
print_color() {
    echo -e "\e[$1m$2\e[0m"
}

# Nom du fichier de sortie CSV pour prologTerm
output_file_prolog="output_prolog.csv"
# Nom du fichier de sortie CSV pour evaluateur
output_file_evaluateur="output_evaluateur.csv"

# Effacer le contenu du fichier de sortie s'il existe déjà et écrire l'en-tête CSV
echo "sample path, correct (yes/no), result" > "$output_file_prolog"
echo "sample path, correct (yes/no), result" > "$output_file_evaluateur"

# Fusionner les fichiers expected.csv et output_evaluateur.csv basés sur la colonne 'path'
# Utiliser awk pour joindre les fichiers sur la colonne 'path'
# Utiliser left join pour garder toutes les entrées de output_evaluateur.csv
# même s'il n'y a pas de correspondance dans expected.csv
awk -F ',' 'BEGIN {OFS = ", "} NR == FNR {expected[$1] = $2; next} {print $0, (expected[$1] ? expected[$1] : "N/A")}' expected.csv output_evaluateur.csv > temp_output.csv

# Renommer temp_output.csv en output_evaluateur.csv
mv temp_output.csv output_evaluateur.csv

# Itérer sur tous les fichiers .aps dans le répertoire Samples et ses sous-répertoires
find Samples -type f -name '*.aps' | while read -r file; do
    # Exécuter prologTerm pour le fichier en cours
    result_prolog=$(./prologTerm "$file" | swipl -s typer.pl -g main_stdin 2>&1)

    # Extraire la sortie standard et la sortie d'erreur
    error_output_prolog=$(echo "$result_prolog" | grep "Erreur")
    standard_output_prolog=$(echo "$result_prolog" | grep -v "Erreur")

    # Vérifier si la sortie standard contient "void"
    if [[ $result_prolog = *"void"* ]]; then
        # Écrire le résultat en tant que ligne CSV avec "yes" pour correct
        echo "$file, yes, Bien typé." >> "$output_file_prolog"
    else
        # Écrire le résultat en tant que ligne CSV avec "no" pour correct
        echo "$file, no, Erreur de typage !!!" >> "$output_file_prolog"
    fi

    # Exécuter evaluateur pour le fichier en cours
    result_evaluateur=$(./evaluateur "$file" 2>&1)

    # Vérifier s'il y a une erreur lors de l'exécution de l'évaluateur
    if [[ $? -eq 0 ]]; then
        # Pas d'erreur, écrire le résultat de l'évaluateur en tant que ligne CSV
        echo "$file, yes, $(echo "$result_evaluateur" | head -n 1), $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected.csv)" >> "$output_file_evaluateur"
    else
        # Erreur lors de l'exécution, écrire l'erreur dans la troisième colonne
        echo "$file, no, $result_evaluateur, $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected.csv)" >> "$output_file_evaluateur"
    fi
done


# Check if the result and expected columns match and update the correct column accordingly
awk -F ', ' 'NR==1 {print $0} NR>1 {if ($3 == $4) $2="yes"; else $2="no"; print $0}' OFS=', ' output_evaluateur.csv > temp_output.csv
mv temp_output.csv output_evaluateur.csv




echo "Le résultat du typage a été enregistré dans le fichier $output_file_prolog au format CSV."
echo "Le résultat de l'évaluation a été enregistré dans le fichier $output_file_evaluateur au format CSV."