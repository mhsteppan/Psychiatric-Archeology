import json
import os
from os.path import join
from ibm_watson import PersonalityInsightsV3
import csv
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator
from strgen import StringGenerator

authenticator = IAMAuthenticator('your API key')
service = PersonalityInsightsV3(
     version='2017-10-13',
     authenticator=authenticator)
service.set_service_url('https://api.eu-gb.personality-insights.watson.cloud.ibm.com/instances/4df3eb28-6eff-4d2d-9f88-6ee123d6a7e1')

# Iteration through all speeches
for filename in os.listdir("/speeches"):
    print(filename)


    with open(join(os.getcwd(), "/speeches/" + filename), 'r',errors='ignore') as \
        profile_json:
        response = service.profile(
        profile_json.read(),
        accept='text/csv',
        csv_headers=True).get_result()

    profile = response.content
    cr = csv.reader(profile.decode('utf-8').splitlines())
    my_list = list(cr)

    f = open('/results/'+filename, 'w')
    writer = csv.writer(f)

    for row in my_list:
        print(row)
        writer.writerow(row)

    f.close()
