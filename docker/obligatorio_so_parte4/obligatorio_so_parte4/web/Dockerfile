FROM node:18-alpine

# Create app directory
WORKDIR /app

# Install dependencies
# Copy package.json and package-lock.json (if present) to leverage Docker cache
COPY package*.json ./

# Install only production dependencies; change to `npm ci` if you have a lockfile and prefer reproducible builds
RUN npm install --production

# Copy app source
COPY . .

# Set production environment
ENV NODE_ENV=production

# App listens on port 5000
EXPOSE 5000

CMD ["npm", "start"]