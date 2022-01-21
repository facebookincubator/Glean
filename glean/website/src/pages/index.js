/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

const features = [
  {
    title: 'Rich types',
    description: <>Store detailed information about code</>,
  },
  {
    title: 'Compact storage',
    description: <>Store data about code at scale</>,
  },
  {
    title: 'Efficient queries',
    description: <>Build experiences with deep insights from code</>,
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="System for collecting, deriving and querying facts about source code">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <img
            className={styles.heroLogo}
            src="img/icon.png"
            alt="Glean Logo"
            width="170"
          />
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={clsx(
                'button button--outline button--secondary button--lg glean',
                styles.buttons,
              )}
              to={useBaseUrl('docs/introduction')}>
              Get Started
            </Link>
          </div>
        </div>
      </header>
      <main>
        <div className="container padding-vert--xl text--left">
          <div className="row">
            <div className="col">
              <h1 className="text--center">Key Features</h1>
              {features && features.length > 0 && (
                <section className={styles.features}>
                  <div className="container">
                    <div className="row">
                      {features.map(({title, imageUrl, description}) => (
                        <Feature
                          key={title}
                          title={title}
                          imageUrl={imageUrl}
                          description={description}
                        />
                      ))}
                    </div>
                  </div>
                </section>
              )}
            </div>
          </div>
        </div>
      </main>
    </Layout>
  );
}

export default Home;
