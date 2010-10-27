/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.telehash.model.TelehashFactory
 * @model kind="package"
 * @generated
 */
public interface TelehashPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "telehash";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://telehash.org/ecore/2010";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "telehash";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	TelehashPackage eINSTANCE = org.telehash.model.impl.TelehashPackageImpl
			.init();

	/**
	 * The meta object id for the '{@link org.telehash.model.impl.TelexImpl <em>Telex</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.model.impl.TelexImpl
	 * @see org.telehash.model.impl.TelehashPackageImpl#getTelex()
	 * @generated
	 */
	int TELEX = 0;

	/**
	 * The feature id for the '<em><b>To</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__TO = 0;

	/**
	 * The feature id for the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__END = 1;

	/**
	 * The feature id for the '<em><b>Line</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__LINE = 2;

	/**
	 * The feature id for the '<em><b>Ring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__RING = 3;

	/**
	 * The feature id for the '<em><b>See</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__SEE = 4;

	/**
	 * The feature id for the '<em><b>Bytes Received</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__BYTES_RECEIVED = 5;

	/**
	 * The number of structural features of the '<em>Telex</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX_FEATURE_COUNT = 6;

	/**
	 * The meta object id for the '{@link org.telehash.model.impl.TapRuleImpl <em>Tap Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.model.impl.TapRuleImpl
	 * @see org.telehash.model.impl.TelehashPackageImpl#getTapRule()
	 * @generated
	 */
	int TAP_RULE = 1;

	/**
	 * The number of structural features of the '<em>Tap Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAP_RULE_FEATURE_COUNT = 0;

	/**
	 * The meta object id for the '<em>Endpoint</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see java.net.InetSocketAddress
	 * @see org.telehash.model.impl.TelehashPackageImpl#getEndpoint()
	 * @generated
	 */
	int ENDPOINT = 2;

	/**
	 * The meta object id for the '<em>Hash</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.Hash
	 * @see org.telehash.model.impl.TelehashPackageImpl#getHash()
	 * @generated
	 */
	int HASH = 3;

	/**
	 * Returns the meta object for class '{@link org.telehash.model.Telex <em>Telex</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Telex</em>'.
	 * @see org.telehash.model.Telex
	 * @generated
	 */
	EClass getTelex();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getTo <em>To</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>To</em>'.
	 * @see org.telehash.model.Telex#getTo()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_To();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getEnd <em>End</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>End</em>'.
	 * @see org.telehash.model.Telex#getEnd()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_End();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getLine <em>Line</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Line</em>'.
	 * @see org.telehash.model.Telex#getLine()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_Line();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getRing <em>Ring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ring</em>'.
	 * @see org.telehash.model.Telex#getRing()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_Ring();

	/**
	 * Returns the meta object for the attribute list '{@link org.telehash.model.Telex#getSee <em>See</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>See</em>'.
	 * @see org.telehash.model.Telex#getSee()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_See();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Bytes Received</em>'.
	 * @see org.telehash.model.Telex#getBytesReceived()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_BytesReceived();

	/**
	 * Returns the meta object for class '{@link org.telehash.model.TapRule <em>Tap Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Tap Rule</em>'.
	 * @see org.telehash.model.TapRule
	 * @generated
	 */
	EClass getTapRule();

	/**
	 * Returns the meta object for data type '{@link java.net.InetSocketAddress <em>Endpoint</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Endpoint</em>'.
	 * @see java.net.InetSocketAddress
	 * @model instanceClass="java.net.InetSocketAddress"
	 * @generated
	 */
	EDataType getEndpoint();

	/**
	 * Returns the meta object for data type '{@link org.telehash.Hash <em>Hash</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Hash</em>'.
	 * @see org.telehash.Hash
	 * @model instanceClass="org.telehash.Hash"
	 * @generated
	 */
	EDataType getHash();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	TelehashFactory getTelehashFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.telehash.model.impl.TelexImpl <em>Telex</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.model.impl.TelexImpl
		 * @see org.telehash.model.impl.TelehashPackageImpl#getTelex()
		 * @generated
		 */
		EClass TELEX = eINSTANCE.getTelex();

		/**
		 * The meta object literal for the '<em><b>To</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__TO = eINSTANCE.getTelex_To();

		/**
		 * The meta object literal for the '<em><b>End</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__END = eINSTANCE.getTelex_End();

		/**
		 * The meta object literal for the '<em><b>Line</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__LINE = eINSTANCE.getTelex_Line();

		/**
		 * The meta object literal for the '<em><b>Ring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__RING = eINSTANCE.getTelex_Ring();

		/**
		 * The meta object literal for the '<em><b>See</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__SEE = eINSTANCE.getTelex_See();

		/**
		 * The meta object literal for the '<em><b>Bytes Received</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__BYTES_RECEIVED = eINSTANCE.getTelex_BytesReceived();

		/**
		 * The meta object literal for the '{@link org.telehash.model.impl.TapRuleImpl <em>Tap Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.model.impl.TapRuleImpl
		 * @see org.telehash.model.impl.TelehashPackageImpl#getTapRule()
		 * @generated
		 */
		EClass TAP_RULE = eINSTANCE.getTapRule();

		/**
		 * The meta object literal for the '<em>Endpoint</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see java.net.InetSocketAddress
		 * @see org.telehash.model.impl.TelehashPackageImpl#getEndpoint()
		 * @generated
		 */
		EDataType ENDPOINT = eINSTANCE.getEndpoint();

		/**
		 * The meta object literal for the '<em>Hash</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.Hash
		 * @see org.telehash.model.impl.TelehashPackageImpl#getHash()
		 * @generated
		 */
		EDataType HASH = eINSTANCE.getHash();

	}

} //TelehashPackage
