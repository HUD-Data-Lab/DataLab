const express = require('express');
const axios = require('axios');

const app = express();
const PORT = 3000;  // proxy server port
const MOCKOON_SERVER = 'https://mock-547c2d70.d1.mockoon.app';  // cloud copy
//const MOCKOON_SERVER = 'https://mock-99ddb204.d1.mockoon.app';  // original version
//const MOCKOON_SERVER = 'http://localhost:3001';  // local

app.use(express.json());  // Middleware to parse JSON bodies

// POST request
app.post('/*', async (req, res) => {
	
	const headers = {
      Authorization: req.headers['authorization'] // Forward the Authorization header
    };
	
  try {
    let modifiedBody; 
    if (req.originalUrl.includes('ceparticipation')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/ceparticipation_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          CEParticipationID: new_id_to_use,
          ...req.body
        };
    } else if (req.originalUrl.includes('clients')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/clients_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
		console.log(new_id_to_use);
        modifiedBody = {
          PersonalID: new_id_to_use,
          ...req.body
        };
    } else if (req.originalUrl.includes('enrollments')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/enrollments_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          EnrollmentID: new_id_to_use,
          ...req.body
        };
    } else if (req.originalUrl.includes('exits')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/exits_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          ExitID: new_id_to_use,
          ...req.body
        };	
    } else if (req.originalUrl.includes('funders')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/funders_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          FunderID: new_id_to_use,
          ...req.body
        };
    } else if (req.originalUrl.includes('organizations')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/organizations_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          OrganizationID: new_id_to_use,
          ...req.body
        };	
    } else if (req.originalUrl.includes('projects')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0/projects_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          ProjectID: new_id_to_use,
          ...req.body
        };
    } else if (req.originalUrl.includes('users')) {
        const maximum_existing_id = await axios.get(`${MOCKOON_SERVER}/users_max_id`, {
      headers: headers,
	  transformResponse: [data => data]
    });
        const new_id_to_use = maximum_existing_id.data + 1;
        modifiedBody = {
          UserID: new_id_to_use,
          ...req.body
        };
    }

    if (!modifiedBody) {
      console.log("No modification made to body, using original request body");
      modifiedBody = { ...req.body };
    } else {
		console.log("Modified body: ", modifiedBody);
	}
    const mockoonResponse = await axios.post(`${MOCKOON_SERVER}/hmis/v1.0${req.originalUrl}`, modifiedBody, {
      headers: headers,
	  transformResponse: [data => data]
    });
    res.status(mockoonResponse.status).send(mockoonResponse.data);  
  } catch (error) {
    if (error.response) {
      res.status(error.response.status).send(error.response.data);
    }
  }
});


// GET request
app.get('/*', async (req, res) => {
  try {
	  const headers = {
      Authorization: req.headers['authorization'] // Forward the Authorization header
    };
	console.log(`${MOCKOON_SERVER}/hmis/v1.0${req.originalUrl}`);
    const mockoonResponse = await axios.get(`${MOCKOON_SERVER}/hmis/v1.0${req.originalUrl}`, {
      headers: headers,
	  transformResponse: [data => data]
    });
	console.log(mockoonResponse.data);
//	res.set(mockoonResponse.headers);
    res.status(mockoonResponse.status).send(mockoonResponse.data);
  } catch (error) {
    if (error.response) {
      res.status(error.response.status).send(error.response.data);
    } else {
      res.sendStatus(500);  // This replaces res.send(500)
    }
  }
  console.log('Request complete');
});



// PATCH request
app.patch('/*', async (req, res) => {
  try {
	  const headers = {
      Authorization: req.headers['authorization'] // Forward the Authorization header
    };
    const mockoonResponse = await axios.patch(`${MOCKOON_SERVER}/hmis/v1.0${req.originalUrl}`, req.body, {
      headers: headers,
	  transformResponse: [data => data]
    });
    res.status(mockoonResponse.status).send(mockoonResponse.data);
  } catch (error) {
	if (error.response) {
      res.status(error.response.status).send(error.response.data);
    }
  }
  console.log('Request complete');
});

// DELETE request
app.delete('/*', async (req, res) => {
  try {
	  const headers = {
      Authorization: req.headers['authorization'] // Forward the Authorization header
    };
    const mockoonResponse = await axios.delete(`${MOCKOON_SERVER}/hmis/v1.0${req.originalUrl}`, {
      headers: headers,
	  transformResponse: [data => data]
    });
    res.status(mockoonResponse.status).send(mockoonResponse.data);
  } catch (error) {
	if (error.response) {
      res.status(error.response.status).send(error.response.data);
    }
  }
  console.log('Request complete');
});

app.listen(PORT, () => {
  console.log(`Proxy server running on http://localhost:${PORT}`);
});